{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Node
    ( nextBlocks
    ) where

import Numeric.Natural
    ( Natural )
import Prelude

import Cardano.Wallet.Node.RustCardano
    ( NetworkLayer (..), newNetworkLayer )
import Cardano.Wallet.Node.Types
    ( EpochIndex, Slot, SlotNumber )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..) )

blockSlot :: BlockHeader -> Slot
blockSlot bh = (epochIndex bh, slotNumber bh)

slotsPerEpoch :: Natural
slotsPerEpoch = 21600

-- | Calculates which epochs to fetch, given a quantity of blocks, and the start
-- point.  Takes into account the latest slot available, and that the most
-- recent epoch is not available in a pack file.
epochRange :: Natural -- ^ Number of blocks
           -> Slot -- ^ Start point
           -> Slot -- ^ Latest slot available
           -> [EpochIndex]
epochRange numBlocks (startEpoch, startSlot) (tipEpoch, _)
    = [startEpoch .. min (tipEpoch - 1) (startEpoch + fromIntegral numEpochs)]
    where
        numEpochs = (numBlocks + fromIntegral startSlot) `div` slotsPerEpoch

-- | Predicate returns true iff the block is from the given slot or a later one.
blockIsAfter :: Slot -> Block -> Bool
blockIsAfter s = blockHeaderIsAfter s . header
    where
        blockHeaderIsAfter :: Slot -> BlockHeader -> Bool
        blockHeaderIsAfter (e, sl) hdr = epochIndex hdr > e
            || (epochIndex hdr == e && slotNumber hdr >= sl)

-- | Predicate returns true iff the block is before the given slot.
blockIsBefore :: Slot -> Block -> Bool
blockIsBefore s = blockHeaderIsBefore s . header
    where
        blockHeaderIsBefore :: Slot -> BlockHeader -> Bool
        blockHeaderIsBefore (e, sl) hdr = epochIndex hdr < e
            || (epochIndex hdr == e && slotNumber hdr < sl)

-- | @blockIsBetween start end@ Returns true if the block is in within the
-- interval @[start, end)@.
blockIsBetween :: Slot -> Slot -> Block -> Bool
blockIsBetween start end b = blockIsAfter start b && blockIsBefore end b

-- | Add a number of slots to an (Epoch, SlotNumber) pair, where the number of
-- slots can be greater than one epoch.
addSlots :: Natural -> Slot -> Slot
addSlots n (e, sl) = (e + fromIntegral e', fromIntegral sl')
    where
        e' = n' `div` slotsPerEpoch
        sl' = n' `mod` slotsPerEpoch
        n' = fromIntegral sl + n

-- fixme: get this from environment
getNetwork :: IO NetworkLayer
getNetwork = newNetworkLayer "mainnet"

-- | Get some blocks from the chain producer.
--
-- This may retrieve less than the requested number of blocks.
-- It might return no blocks at all.
nextBlocks
    :: forall m. (m ~ IO)
    => Natural -- ^ Number of blocks to retrieve
    -> Slot -- ^ Starting point
    -> m [Block]
--    -> ExceptT ErrGetNextBlocks m [Block]
nextBlocks numBlocks start = do
    network <- getNetwork
    tip <- getNetworkTip network
    -- grab blocks from epoch pack files
    let epochs = epochRange numBlocks start (blockSlot tip)
        end = addSlots numBlocks start
    epochBlocks <- concat <$> mapM (getEpoch network) epochs
    -- grab remaining blocks from the latest epoch if necessary
    lastBlocks <- if null epochs || fst end > maximum epochs
        then fetchBlocksFromTip network start tip
        else pure []
    -- return blocks in order
    pure $ filter (blockIsBetween start end) epochBlocks ++ filter (blockIsBefore end) lastBlocks

-- Note: this is working around a limitation of the cardano-http-bridge API.
-- fixme: it does not fetch the tip block itself.
fetchBlocksFromTip :: NetworkLayer -> Slot -> BlockHeader -> IO [Block]
fetchBlocksFromTip network start tip = reverse <$> workBackwards tip
    where
        workBackwards bh = do
            block <- getBlock network (prevBlockHash bh)
            putStrLn $ "Got block " <> (show $ epochIndex (header block)) <> "." <> (show $ slotNumber (header block))
            if epochIndex (header block) /= epochIndex tip || not (blockIsAfter start block)
                then pure []
                else do
                    blocks <- workBackwards (header block)
                    pure (block:blocks)
