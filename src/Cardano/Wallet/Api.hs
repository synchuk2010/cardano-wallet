{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Cardano.Wallet.Api.Types
    ( Wallet, WalletId, WalletPostData )
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Delete, Get, JSON, NoContent, Post, ReqBody )

api :: Proxy Api
api = Proxy

type Api =
    DeleteWallet
    :<|> GetWallet
    :<|> ListWallets
    :<|> PostWallet

type DeleteWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Delete '[] NoContent

type GetWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Get '[JSON] Wallet

type ListWallets = "wallets"
    :> Get '[JSON] [Wallet]

type PostWallet = "wallets"
    :> ReqBody '[JSON] WalletPostData
    :> Post '[JSON] Wallet
