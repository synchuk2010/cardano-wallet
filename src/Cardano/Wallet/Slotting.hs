{-# LANGUAGE DeriveGeneric              #-}

module Cardano.Wallet.Slotting
    ( SlotId (..)
    , EpochIndex
    , LocalSlotIndex
    , SlotCount
    , slotsPerEpoch
    , addSlots
    ) where

import Prelude

import Data.Word (Word16, Word64)
import Numeric.Natural
    ( Natural )
import GHC.Generics (Generic)

-- fixme: would like to use overloaded record fields
data SlotId = SlotId
  { siEpoch :: !EpochIndex
  , siSlot :: !LocalSlotIndex
  } deriving (Show, Eq, Ord, Generic)

type EpochIndex = Word64
type LocalSlotIndex = Word16
type SlotCount = Natural

-- | Hard-coded for the time being
slotsPerEpoch :: SlotCount
slotsPerEpoch = 21600

-- | Add a number of slots to an (Epoch, SlotNumber) pair, where the number of
-- slots can be greater than one epoch.
addSlots :: SlotCount -> SlotId -> SlotId
addSlots n (SlotId e sl) = SlotId (e + fromIntegral e') (fromIntegral sl')
    where
        e' = n' `div` slotsPerEpoch
        sl' = n' `mod` slotsPerEpoch
        n' = fromIntegral sl + n
