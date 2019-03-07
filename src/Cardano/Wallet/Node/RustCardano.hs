{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Node.RustCardano
    ( NetworkConfig(..)
    , NetworkLayer(..)
    , newNetworkLayer
    ) where

import Data.Text
    ( Text )
import Prelude

import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
    ( Manager
    , defaultRequest
    , httpLbs
    , method
    , path
    , port
    , requestBody
    , responseBody
    , responseStatus
    )
import qualified Network.HTTP.Client as HTTP

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Exception
    ( throwIO )
import Control.Monad
    ( void )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.Word
    ( Word16, Word64 )

import Cardano.Wallet.Binary
import Cardano.Wallet.Binary.Packfile
    ( PackfileError (..), decodePackfile )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )

-- newtype NetworkName = NetworkName Text
--     deriving (Show, Eq)
type NetworkName = Text

data NetworkConfig = NetworkConfig
    { cfgNetworkName :: NetworkName
    , cfgHost :: ByteString
    , cfgPort :: Word16
    } deriving (Show)

-- | Endpoints of the cardano-http-bridge API.
data NetworkLayer = NetworkLayer
    { getBlock      :: Hash "BlockHeader" -> IO Block
    , getEpoch      :: Word64 -> IO [Block]
    , getNetworkTip :: IO BlockHeader
    , postSignedTx  :: BS.ByteString -> IO ()
    }


hashToBase16 :: Hash a -> ByteString
hashToBase16 (Hash h) = convertToBase Base16 h

mkNetworkLayer :: NetworkName -> Manager -> NetworkLayer
mkNetworkLayer network manager = NetworkLayer
    { getBlock = _getBlock network manager . hashToBase16
    , getEpoch = _getEpoch network manager
    , getNetworkTip = _getNetworkTip network manager
    , postSignedTx = undefined -- _postSignedTx network manager
    }


newNetworkLayer :: NetworkName -> IO NetworkLayer
newNetworkLayer network = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    return $ mkNetworkLayer network manager


_getBlock :: NetworkName -> Manager -> ByteString -> IO Block
_getBlock network manager hash = do
    let req = defaultRequest
            { port = 1337
            , path = "/" <> T.encodeUtf8 network <> "/block/" <> hash
            }
    res <- httpLbs req manager
    let block = unsafeDeserialiseFromBytes decodeBlock $ responseBody res
    return block

{-
decodePackfile :: BL.ByteString -> Either String [BS.ByteString]
decodePackfile = undefined -- in another branch
-}

deserialiseEpoch :: BL.ByteString -> Either PackfileError [Block]
deserialiseEpoch = fmap (map (unsafeDeserialiseFromBytes decodeBlock . BL.fromStrict)) . decodePackfile

_getEpoch :: NetworkName -> Manager -> Word64 -> IO [Block]
_getEpoch network manager n = do
    let req = defaultRequest
            { port = 1337
            , path = "/" <> T.encodeUtf8 network <> "/epoch/" <> B8.pack (show n)
            }
    res <- httpLbs req manager
    case deserialiseEpoch (responseBody res) of
        Left e -> error $ "error decoding epoch: " <> show e
        Right blocks -> pure blocks


_getNetworkTip :: NetworkName -> Manager -> IO BlockHeader
_getNetworkTip network manager = do
    let req = defaultRequest
            { port = 1337
            , path = "/" <> T.encodeUtf8 network <> "/tip"
            }
    res <- httpLbs req manager
    let tip = unsafeDeserialiseFromBytes decodeBlockHeader $ responseBody res
    return tip

{-
_postSignedTx :: NetworkName -> Manager -> BS.ByteString -> IO ()
_postSignedTx network manager tx = do
    let req = defaultRequest
            { port = 1337
            , method = "POST"
            , path = "/" <> T.encodeUtf8 network <> "/txs/signed"
            , requestBody = HTTP.RequestBodyBS $ convertToBase Base64 tx
            }
    void $ httpLbs req manager
-}

unsafeDeserialiseFromBytes :: (forall s. CBOR.Decoder s a) -> BL.ByteString -> a
unsafeDeserialiseFromBytes decoder bytes =
    either (\e -> error $ "unsafeDeserialiseFromBytes: " <> show e) snd $
        CBOR.deserialiseFromBytes decoder bytes
