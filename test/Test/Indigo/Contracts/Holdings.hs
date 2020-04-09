-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Test.Indigo.Contracts.Holdings
  ( test_setNameOrSymbol
  , test_setSafelistAddress
  , test_adminRightsTransfer
  , test_mint
  , test_approve
  , test_transfer
  , test_burnAndBurnAll
  , test_setPauseAndSetTransferable
  , test_documentation
  , unit_FA1'2_is_implemented
  , unit_nettest_scenario
  , unit_whitelist_integration
  ) where

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import Lorentz (Address, TAddress(..), ToAddress(..), mkView, mt)
import qualified Lorentz.Contracts.ManagedLedger.Types as ML
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Test
import Michelson.Runtime (prepareContract)
import Michelson.Runtime.GState (genesisAddress, genesisAddress1, genesisAddress2)
import Morley.Nettest
import Tezos.Core (toMutez)
import Util.Named ((.!))

import Indigo.Contracts.Holdings

import Nettest.Holdings
import Nettest.WhitelistIntegration
import Test.Indigo.Contracts.Common

originateHoldings
  :: Address -> Maybe Address -> IntegrationalScenarioM (TAddress Parameter)
originateHoldings owner mbSafelist =
  lOriginate holdingsContract "holdings"
  (mkStorage owner owner mbSafelist dummyMeta) (toMutez 0)

ownerAddress :: Address
ownerAddress = genesisAddress

test_setNameOrSymbol :: TestTree
test_setNameOrSymbol = testGroup "Test SetName and SetSymbol entrypoints"
  [ testCase "Successfully call SetName and SetSymbol" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h (SetName $ #newName .! [mt|TokenBek|])
      withSender ownerAddress $ lCallDef h (SetSymbol $ #newSymbol .! [mt|Bek|])
      lCallDef h $ GetTokenMeta (mkView () consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer
        [dummyMeta {tmName = [mt|TokenBek|], tmSymbol = [mt|Bek|]} ]
  , testCase "Call SetName without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender genesisAddress2 $ lCallDef h (SetName $ #newName .! [mt|TokenBek|])
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call SetSymbol without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender genesisAddress2 $ lCallDef h (SetSymbol $ #newSymbol .! [mt|Bek|])
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call SetName when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h (SetName $ #newName .! [mt|TokenBek|])
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  , testCase "Call SetName when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h (SetSymbol $ #newSymbol .! [mt|Bek|])
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  ]

test_setSafelistAddress :: TestTree
test_setSafelistAddress = testGroup "Test SetSafelistAddress entypoint"
  [ testCase "Successfully call SetSafelistAddress" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      sl <- originateSafelist [] []
      withSender ownerAddress $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just $ toAddress sl)
      validate . Right $ expectStorageUpdate h
        (\st ->
           sfMbSafelistAddress (ML.fields st) == Just (toAddress sl)
        ) "Unexpected new safelist address"
  , testCase "Call without owner rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      sl <- originateSafelist [] []
      withSender genesisAddress2 $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just $ toAddress sl)
      validate . Left $ lExpectCustomError_ #senderIsNotOwner
  , testCase "New safelist has incorrect parameter type" $
      integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      let newSafelist = toAddress h
      withSender ownerAddress $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just newSafelist)
      validate . Left $ lExpectCustomError #invalidSafelistAddr [mt|assertTransfer|]
  ]

test_adminRightsTransfer :: TestTree
test_adminRightsTransfer = testGroup "Test admin rights transfer"
  [ testCase "Successfully transfer admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      lCallDef h $ IsAdmin (mkView ownerAddress consumer)
      withSender ownerAddress $ lCallDef h $
        TransferAdminRights $ #newAdmin .! genesisAddress1
      withSender genesisAddress1 $ lCallDef h $ AcceptAdminRights ()
      lCallDef h $ IsAdmin (mkView ownerAddress consumer)
      lCallDef h $ IsAdmin (mkView genesisAddress1 consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [True, False, True]
  , testCase "Call TransferAdminRights without owner rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender genesisAddress1 $ lCallDef h $
        TransferAdminRights $ #newAdmin .! genesisAddress1
      validate . Left $ lExpectCustomError_ #senderIsNotOwner
  , testCase "Call AcceptAdminRight from wrong address" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h $
        TransferAdminRights $ #newAdmin genesisAddress1
      withSender genesisAddress2 $ lCallDef h $ AcceptAdminRights ()
      validate . Left $ lExpectCustomError_ #senderIsNotNewAdmin
  , testCase "Call TransferAdminRight when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h $
        TransferAdminRights $ #newAdmin .! genesisAddress1
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  ]

test_mint :: TestTree
test_mint = testGroup "Test Mint entrypoint"
  [ testCase "Successful Mint" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [100]
  , testCase "Successful Mint without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [100]
  , testCase "Call Mint without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender genesisAddress1 $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call Mint for nonReceiver address" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] []
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      validate . Left $
        lExpectCustomError_ #assertionFailure
  , testCase "Call Mint when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  ]

test_approve :: TestTree
test_approve = testGroup "Test Approve entrypoint"
  [ testCase "Successful Approve" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress, genesisAddress1]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [100]
  , testCase "Successful Approve without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [100]
  , testCase "Approve for nonReceiver spender fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress1]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      validate . Left $
        lExpectCustomError_ #assertionFailure
  , testCase "Approve for nonReceiver sender fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      validate . Left $
        lExpectCustomError_ #assertionFailure
  , testCase "Call Approve when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  ]

test_transfer :: TestTree
test_transfer = testGroup "Test Transfer entrypoint"
  [ testCase "Successful Transfer" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [(ownerAddress, genesisAddress1)] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [30, 70]
  , testCase "Successful Transfer without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [30, 70]
  , testCase "Successful Transfer without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $
        Transfer (#from .! genesisAddress1, #to .! ownerAddress, #value .! 70)
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Transfer when safelist doesn't allow required pair fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      validate . Left $
        lExpectCustomError_ #assertionFailure
  , testCase "Transfer more tokens that have" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 150)
      validate . Left $
        lExpectCustomError #notEnoughBalance (#required .! 150, #present .! 100)
  , testCase "Succesful tranfer other tokens" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 70)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! genesisAddress1, #to .! ownerAddress, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [70, 30]
  , testCase "Transfer more tokens than approved" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 70)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! genesisAddress1, #to .! ownerAddress, #value .! 100)
      validate . Left $
        lExpectCustomError #notEnoughAllowance (#required .! 100, #present .! 70)
  , testCase "Transfer when token is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  , testCase "Transfer when token is not transferable" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetTransferable $ #value .! False)
      withSender ownerAddress $
        lCallDef h $
        Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      validate . Left $ lExpectCustomError_ #nonTransferable
  ]

test_burnAndBurnAll :: TestTree
test_burnAndBurnAll = testGroup "Test Burn and BurnAll entrypoints"
  [ testCase "Successful calls" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 70)
      withSender ownerAddress $ lCallDef h $ Burn (#from .! ownerAddress, #value .! 50)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      withSender ownerAddress $ lCallDef h $ BurnAll ()
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [70, 50, 0, 0]
  , testCase "Burn more tokens that have" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h $ Burn (#from .! ownerAddress, #value .! 150)
      validate . Left $
        lExpectCustomError #notEnoughBalance (#required .! 150, #present .! 100)
  , testCase "Call Burn without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender genesisAddress1 $ lCallDef h $
        Burn (#from .! ownerAddress, #value .! 50)
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call BurnAll without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender genesisAddress1 $ lCallDef h $ BurnAll ()
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call Burn when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h $
        Burn (#from .! ownerAddress, #value .! 50)
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  , testCase "Call BurnAll when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      withSender ownerAddress $ lCallDef h $ BurnAll ()
      validate . Left $ lExpectCustomError_ #tokenOperationsArePaused
  ]

test_setPauseAndSetTransferable :: TestTree
test_setPauseAndSetTransferable = testGroup "Test SetPause and SetTransferable entrypoints"
  [ testCase "Call SetPause without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender genesisAddress1 $ lCallDef h (SetPause $ #value .! True)
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Call SetPause without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender genesisAddress1 $ lCallDef h (SetTransferable $ #value .! True)
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  ]

unit_FA1'2_is_implemented :: Assertion
unit_FA1'2_is_implemented =
  expectContractEntrypoints @AL.Parameter holdingsContract

test_documentation :: [TestTree]
test_documentation = runDocTests testLorentzDoc holdingsDoc

unit_nettest_scenario :: Expectation
unit_nettest_scenario =
  integrationalTestExpectation $ nettestToIntegrational nettestScenario

unit_whitelist_integration :: Expectation
unit_whitelist_integration = do
  whitelistContract <- prepareContract $ Just "resources/whitelist.tz"
  integrationalTestExpectation $
    nettestToIntegrational (whitelistScenario whitelistContract)
