module Cardano.Wallet.ChainProducer.RustCardanoSpec (spec) where

import Prelude

import Control.Exception
    ( Exception, throwIO )
import Control.Monad.Except
import Control.Monad.IO.Class
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec
    ( SpecWith (..) )

import Cardano.Wallet.ChainProducer.MockNetworkLayer
import Cardano.Wallet.ChainProducer.RustCardano
import Cardano.Wallet.ChainProducer.TempNetwork
    ( NetworkLayer, newNetworkLayer )
import Cardano.Wallet.Slotting
    ( EpochIndex, SlotId (..), slotPrev, slotsPerEpoch )

unsafeRunExceptT :: (Exception e, MonadIO m) => ExceptT e m a -> m a
unsafeRunExceptT = either (liftIO . throwIO) pure <=< runExceptT

spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        let network = mockNetworkLayer 104 (SlotId 106 1492)
        getNextBlocksSpec network

    {-
    mainnet <- liftIO $ newNetworkLayer "mainnet"
    describe "Getting next blocks with a real backend" $
        getNextBlocksSpec mainnet
    -}

getNextBlocksSpec :: NetworkLayer -> SpecWith ()
getNextBlocksSpec network = do
    let run = runRustBackend network . unsafeRunExceptT

    it "should get something from the latest epoch" $ do
        blocks <- run $ nextBlocks 1000 (SlotId 105 17000)
        length blocks `shouldBe` 1000

    it "should get from old epochs" $ do
        bs <- run $ nextBlocks 1000 (SlotId 104 10000)
        length bs `shouldBe` 1000
