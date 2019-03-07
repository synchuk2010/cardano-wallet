-- | Useful extra functions related to CBOR processing.
--
module Codec.CBOR.Extra
    ( FromCBOR (..)
    ) where

import qualified Codec.CBOR.Decoding as CBOR

-- | The class of types that can be converted to from CBOR.
--
class FromCBOR a where
    fromCBOR :: CBOR.Decoder s a

