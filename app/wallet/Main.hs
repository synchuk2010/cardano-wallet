{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Data.Text
    ( Text )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , command
    , docoptFile
    , exitWithUsage
    , getArg
    , getArgOrExitWith
    , isPresent
    , longOption
    , parseArgsOrExit
    )
import System.Environment
    ( getArgs )

import qualified Data.Text as T

patterns :: Docopt
patterns = [docoptFile|app/wallet/USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO Text
getArgOrExit as o = T.pack <$> getArgOrExitWith patterns as o

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    print =<< parseCommand args

parseCommand :: Arguments -> IO Command
parseCommand args
    | args `isPresent` command "address" =
        AddressCommand
            <$> parseAddressCommand args
    | args `isPresent` command "wallet" =
        WalletCommand
            <$> parseWalletCommand args
    | otherwise =
        exitWithUsage patterns

parseAddressCommand :: Arguments -> IO AddressCommand
parseAddressCommand args
    | args `isPresent` command "list" =
        AddressList <$> addressListOptions
    | otherwise =
        exitWithUsage patterns
  where
    addressListOptions = AddressListOptions
        <$> fmap WalletId (args `getArgOrExit` longOption "wallet-id")

parseWalletCommand :: Arguments -> IO WalletCommand
parseWalletCommand args
    | args `isPresent` command "create" =
        WalletCreate <$> walletCreateOptions
    | args `isPresent` command "delete" =
        WalletDelete <$> walletDeleteOptions
    | args `isPresent` command "list" =
        pure WalletList
    | args `isPresent` command "update" =
        WalletUpdate <$> walletUpdateOptions
    | otherwise =
        exitWithUsage patterns
  where
    walletCreateOptions = WalletCreateOptions
        <$> pure (T.pack <$> args `getArg` longOption "address-pool-gap")
        <*> args `getArgOrExit` longOption "mnemonic-sentence"
        <*> pure (T.pack <$> args `getArg` longOption "mnemonic-second-factor")
        <*> fmap WalletName (args `getArgOrExit` longOption "name")
        <*> args `getArgOrExit` longOption "passphrase"
    walletDeleteOptions = WalletDeleteOptions
        <$> fmap WalletId (args `getArgOrExit` longOption "id")
    walletUpdateOptions = WalletUpdateOptions
        <$> fmap WalletId (args `getArgOrExit` longOption "id")
        <*> fmap WalletName (args `getArgOrExit` longOption "name")

data Command
    = AddressCommand AddressCommand
    | WalletCommand WalletCommand
    deriving (Eq, Show)

newtype AddressCommand
    = AddressList AddressListOptions
    deriving (Eq, Show)

newtype AddressListOptions = AddressListOptions
    { walletId :: WalletId
    } deriving (Eq, Show)

data WalletCommand
    = WalletCreate WalletCreateOptions
    | WalletDelete WalletDeleteOptions
    | WalletList
    | WalletUpdate WalletUpdateOptions
    deriving (Eq, Show)

data WalletCreateOptions = WalletCreateOptions
    { addressPoolGap :: Maybe Text
    , mnemonicSentence :: Text
    , mnemonicSecondFactor :: Maybe Text
    , name :: WalletName
    , passphrase :: Text
    } deriving (Eq, Show)

newtype WalletDeleteOptions = WalletDeleteOptions
    { id :: WalletId
    } deriving (Eq, Show)

data WalletUpdateOptions = WalletUpdateOptions
    { id :: WalletId
    , name :: WalletName
    } deriving (Eq, Show)

newtype WalletId = WalletId Text
    deriving (Eq, Show)

newtype WalletName = WalletName Text
    deriving (Eq, Show)
