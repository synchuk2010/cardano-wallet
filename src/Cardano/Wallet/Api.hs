{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Cardano.Wallet.Api.Types
    ( Address
    , AddressState
    , ApiT
    , Wallet
    , WalletId
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>)
    , (:>)
    , Capture
    , Delete
    , Get
    , JSON
    , NoContent
    , Post
    , Put
    , QueryParam
    , ReqBody
    )

api :: Proxy Api
api = Proxy

type Api = Addresses :<|> Wallets

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses =
    ListAddresses

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses = "wallets"
    :> Capture "walletId" WalletId
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [Address]

{-------------------------------------------------------------------------------
                                  Wallets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Wallets
-------------------------------------------------------------------------------}

type Wallets =
    DeleteWallet
    :<|> GetWallet
    :<|> ListWallets
    :<|> PostWallet
    :<|> PutWallet
    :<|> PutWalletPassphrase

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteWallet
type DeleteWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Delete '[] NoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Get '[JSON] Wallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [Wallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] WalletPostData
    :> Post '[JSON] Wallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWallet
type PutWallet = "wallets"
    :> Capture "walletId" WalletId
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] Wallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWalletPassphrase
type PutWalletPassphrase = "wallets"
    :> Capture "walletId" WalletId
    :> ReqBody '[JSON] WalletPutPassphraseData
    :> Put '[] NoContent