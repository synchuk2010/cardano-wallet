module Cardano.Wallet.Node.Types
    ( EpochIndex, SlotNumber, Slot ) where

import Prelude
import Data.Word (Word64, Word16)

data ErrGetNextBlocks
    = ErrGetNextBlocks
    deriving (Show, Eq)

-- fixme: add to Primitive types
type EpochIndex = Word64
type SlotNumber = Word16
type Slot = (EpochIndex, SlotNumber)
