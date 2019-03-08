{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.ChainProducer.RustCardano
    ( RustBackend
    , runRustBackend
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Exception (Exception, tryJust)
import Prelude

import Cardano.Wallet.ChainProducer
import Cardano.Wallet.ChainProducer.RustCardano.NetworkLayer
    ( NetworkLayer (..), NetworkLayerError )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..) )
import Cardano.Wallet.Slotting
    ( EpochIndex, SlotCount, SlotId (..), addSlots, slotsPerEpoch, slotNext )

blockSlot :: BlockHeader -> SlotId
blockSlot bh = SlotId (epochIndex bh) (slotNumber bh)

-- | Calculates which epochs to fetch, given a quantity of blocks, and the start
-- point.  Takes into account the latest slot available, and that the most
-- recent epoch is not available in a pack file.
epochRange :: SlotCount -- ^ Number of blocks
           -> SlotId -- ^ Start point
           -> SlotId -- ^ Latest slot available
           -> [EpochIndex]
epochRange numBlocks (SlotId startEpoch startSlot) (SlotId tipEpoch _)
    = [startEpoch .. min (tipEpoch - 1) (startEpoch + fromIntegral numEpochs)]
    where
        numEpochs = (numBlocks + fromIntegral startSlot) `div` slotsPerEpoch

-- | Predicate returns true iff the block is from the given slot or a later one.
blockIsAfter :: SlotId -> Block -> Bool
blockIsAfter s = (>= s) . blockSlot . header

-- | Predicate returns true iff the block is before the given slot.
blockIsBefore :: SlotId -> Block -> Bool
blockIsBefore s = (< s) . blockSlot . header

-- | @blockIsBetween start end@ Returns true if the block is in within the
-- interval @[start, end)@.
blockIsBetween :: SlotId -> SlotId -> Block -> Bool
blockIsBetween start end b = blockIsAfter start b && blockIsBefore end b

newtype RustBackend a = RustBackend {
    runRB :: ReaderT NetworkLayer IO a
    } deriving (Monad, Applicative, Functor, MonadReader NetworkLayer, MonadIO)

runRustBackend :: NetworkLayer -> RustBackend a -> IO a
runRustBackend network action = runReaderT (runRB action) network

getNetwork :: RustBackend NetworkLayer
getNetwork = ask

instance MonadChainProducer RustBackend where
    nextBlocks = rbNextBlocks

-- fixme: This will be quite inefficient for at least two reasons.
-- 1. If the number of blocks is small, it will fetch the same epoch pack file
--    repeatedly.
-- 2. Fetching the tip block and working backwards is not ideal.
-- We will keep it for now, and it can be improved later.
rbNextBlocks
    :: SlotCount -- ^ Number of blocks to retrieve
    -> SlotId    -- ^ Starting point
    -> ExceptT ErrGetNextBlocks RustBackend [Block]
rbNextBlocks numBlocks start = handleBackendErrors $ do
    network <- getNetwork
    tip <- liftIO $ getNetworkTip network

    -- grab blocks from epoch pack files
    let epochs = epochRange numBlocks start (blockSlot tip)
        end = addSlots numBlocks start
    epochBlocks <- concat <$> liftIO (getEpochs network epochs)

    -- find the start point after packed epochs
    let start' = if null epochBlocks
                     then start
                     else slotNext . blockSlot . header . last $ epochBlocks

    -- grab remaining blocks from the latest epoch if necessary
    lastBlocks <- if end > start'
        then liftIO $ fetchBlocksFromTip network start' tip
        else pure []

    -- return blocks in order, excluding blocks outside the given range
    let epochBlocks' = filter (blockIsBetween start end) epochBlocks
        lastBlocks' = filter (blockIsBefore end) lastBlocks
    pure (epochBlocks' ++ lastBlocks')

-- Fetch epoch blocks until one fails.
getEpochs :: NetworkLayer -> [EpochIndex] -> IO [[Block]]
getEpochs network = mapUntilError selector (getEpoch network)
    where
        selector :: NetworkLayerError -> Maybe ()
        selector = const (Just ())

-- | Apply an IO action to each element of a list, until an action fails, or
-- there are no more elements. This is like mapM, except that the resulting list
-- might be smaller than the given list.
mapUntilError
    :: Exception e
    => (e -> Maybe ()) -- ^ Error handler
    -> (a -> IO b) -- ^ Action to run
    -> [a] -- ^ Elements
    -> IO [b] -- ^ Results
mapUntilError h get (e:es) = tryJust h (get e) >>= \case
    Left () -> pure []
    Right r -> do
        rs <- mapUntilError h get es
        pure (r:rs)
mapUntilError _ _ [] = pure []

-- Fetch blocks which are not in epoch pack files.
-- fixme: it does not fetch the tip block itself.
fetchBlocksFromTip :: NetworkLayer -> SlotId -> BlockHeader -> IO [Block]
fetchBlocksFromTip network start tip = reverse <$> workBackwards tip
    where
        workBackwards bh = do
            block <- getBlock network (prevBlockHash bh)
            if blockIsAfter start block
                then do
                    blocks <- workBackwards (header block)
                    pure (block:blocks)
                else pure []

-- This is just a placeholder function. The network layer functions should
-- probably use ExceptT.
handleBackendErrors :: RustBackend a -> ExceptT ErrGetNextBlocks RustBackend a
handleBackendErrors = ExceptT . fmap Right
