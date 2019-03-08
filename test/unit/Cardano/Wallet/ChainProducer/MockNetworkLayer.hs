module Cardano.Wallet.ChainProducer.MockNetworkLayer
    ( mockNetworkLayer
    ) where

import Prelude

import Control.Exception
    ( throwIO )
import qualified Data.ByteString.Char8 as S8

import Cardano.Wallet.ChainProducer.RustCardano.NetworkLayer
    ( NetworkLayer (..), NetworkLayerError (..) )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Cardano.Wallet.Slotting
    ( EpochIndex, SlotId (..), slotPrev, slotsPerEpoch )

-- | Embed an epoch index and slot number into a hash.
mockHash :: SlotId -> Hash a
mockHash (SlotId ep sl) = Hash $ S8.pack ("Hash " <> show ep <> "." <> show sl)

-- | Extract the epoch index and slot number from a hash.
unMockHash :: Hash a -> SlotId
unMockHash (Hash h) = parse . map S8.unpack . S8.split '.' . S8.drop 5 $ h
    where
        parse [ep, sl] = SlotId (read ep) (read sl)
        parse _ = error $ "Could not read mock hash: " ++ S8.unpack h

-- | Create a block header from its hash, assuming that the hash was created
-- with 'mockHash'.
mockHeaderFromHash :: Hash a -> BlockHeader
mockHeaderFromHash h = BlockHeader (fromIntegral ep) (fromIntegral sl) prevHash
    where
        slotId@(SlotId ep sl) = unMockHash h
        prevHash = mockHash (slotPrev slotId)

-- | Generate an entire epoch's worth of mock blocks. There are no transactions
-- generated.
mockEpoch :: EpochIndex -> [Block]
mockEpoch ep = [ Block (mockHeaderFromHash (mockHash sl)) mempty
               | sl <- [ SlotId ep (fromIntegral i) | i <- epochs ] ]
    where epochs = [ 0 .. fromIntegral (slotsPerEpoch - 1) ]

-- | A network layer which returns mock blocks.
mockNetworkLayer
    :: EpochIndex -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> NetworkLayer
mockNetworkLayer firstUnstableEpoch tip = NetworkLayer
    { getBlock = \hash -> do
            -- putStrLn $ "mock getBlock " ++ show hash
            pure $ Block (mockHeaderFromHash hash) mempty
    , getEpoch = \ep -> do
            -- putStrLn $ "mock getEpoch " ++ show ep
            if ep < firstUnstableEpoch
                then pure $ mockEpoch $ fromIntegral ep
                else throwIO $ DeserialiseError $ "mock epoch " ++ show ep ++ " > firstUnstableEpoch " ++ show firstUnstableEpoch
    , getNetworkTip = do
            -- putStrLn $ "mock getNetworkTip"
            pure $ mockHeaderFromHash (mockHash tip)
    }
