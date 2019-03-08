{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.ChainProducer.RustCardano.NetworkLayer
    ( NetworkLayer (..)
    , NetworkLayerError(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Control.Exception
    ( Exception (..) )
import Data.Word
    ( Word64 )

-- | Endpoints of the cardano-http-bridge API.
data NetworkLayer = NetworkLayer
    { getBlock      :: Hash "BlockHeader" -> IO Block
    , getEpoch      :: Word64 -> IO [Block]
    , getNetworkTip :: IO BlockHeader
    }

data NetworkLayerError
    = DeserialiseError String
    deriving (Show, Eq)

instance Exception NetworkLayerError
