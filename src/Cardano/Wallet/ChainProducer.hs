module Cardano.Wallet.ChainProducer
    ( MonadChainProducer (..)
    , ErrGetNextBlocks (..)
    ) where

import Prelude

import Control.Exception ( Exception )
import Control.Monad.Except ( ExceptT )
import Cardano.Wallet.Slotting ( SlotId, SlotCount )
import Cardano.Wallet.Primitive ( Block )

class MonadChainProducer m where
    -- | Get some blocks from the chain producer.
    --
    -- This may retrieve less than the requested number of blocks.
    -- It might return no blocks at all.
    nextBlocks
        :: SlotCount -- ^ Number of blocks to retrieve
        -> SlotId    -- ^ Starting point
        -> ExceptT ErrGetNextBlocks m [Block]

-- | The things that can go wrong when retrieving blocks.
data ErrGetNextBlocks
    = GetNextBlocksError
    deriving (Show, Eq)

instance Exception ErrGetNextBlocks
