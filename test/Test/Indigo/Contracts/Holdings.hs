-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Test.Indigo.Contracts.Holdings
  ( test_setNameOrSymbol
  , test_setSafelistAddress
  , test_adminRightsTransfer
  , test_mint
  , test_approve
  , test_transfer
  , test_seize
  , test_burnAndBurnAll
  , test_setPauseAndSetTransferable
  , test_documentation
  , unit_FA1'2_is_implemented
  , test_approvableLedger
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit (Assertion, testCase)

import Lorentz (Address, ToAddress(..), mkView, mt)
import qualified Lorentz.Contracts.ManagedLedger.Types as ML
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Test (approvableLedgerSpec)
import Lorentz.Test
import Util.Named ((.!))

import Indigo.Contracts.Holdings

import Test.Indigo.Contracts.Common

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
      lExpectViewConsumerStorage consumer
        [dummyMeta {tmName = [mt|TokenBek|], tmSymbol = [mt|Bek|]} ]
  , testCase "Call SetName without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      err <- expectError $
        withSender genesisAddress2 $ lCallDef h (SetName $ #newName .! [mt|TokenBek|])
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call SetSymbol without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      err <- expectError $ withSender genesisAddress2 $ lCallDef h (SetSymbol $ #newSymbol .! [mt|Bek|])
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call SetName when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $ lCallDef h (SetName $ #newName .! [mt|TokenBek|])
      lExpectCustomError_ #tokenOperationsArePaused err
  , testCase "Call SetSymbol when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $ lCallDef h (SetSymbol $ #newSymbol .! [mt|Bek|])
      lExpectCustomError_ #tokenOperationsArePaused err
  ]

test_setSafelistAddress :: TestTree
test_setSafelistAddress = testGroup "Test SetSafelistAddress entypoint"
  [ testCase "Successfully call SetSafelistAddress" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      sl <- originateSafelist [] []
      withSender ownerAddress $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just $ toAddress sl)
      expectStorageUpdate h
        (\st ->
           sfMbSafelistAddress (ML.fields st) == Just (toAddress sl)
        ) "Unexpected new safelist address"
  , testCase "Call without owner rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      sl <- originateSafelist [] []
      err <- expectError $
        withSender genesisAddress2 $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just $ toAddress sl)
      lExpectCustomError_ #senderIsNotOwner err
  , testCase "New safelist has incorrect parameter type" $
      integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      let newSafelist = toAddress h
      err <- expectError $
        withSender ownerAddress $ lCallDef h $ SetSafelistAddress $
        #newMbSafelistAddress .! (Just newSafelist)
      lExpectCustomError #invalidSafelistAddr [mt|assertTransfer|] err
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
      lExpectViewConsumerStorage consumer [True, False, True]
  , testCase "Call TransferAdminRights without owner rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      err <- expectError $
        withSender genesisAddress1 $ lCallDef h $
        TransferAdminRights $ #newAdmin .! genesisAddress1
      lExpectCustomError_ #senderIsNotOwner err
  , testCase "Call AcceptAdminRight from wrong address" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h $
        TransferAdminRights $ #newAdmin genesisAddress1
      err <- expectError $
        withSender genesisAddress2 $ lCallDef h $ AcceptAdminRights ()
      lExpectCustomError_ #senderIsNotNewAdmin err
  , testCase "Call TransferAdminRight when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $ lCallDef h $
        TransferAdminRights $ #newAdmin .! genesisAddress1
      lExpectCustomError_ #tokenOperationsArePaused err
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
      lExpectViewConsumerStorage consumer [100]
  , testCase "Successful Mint without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lExpectViewConsumerStorage consumer [100]
  , testCase "Call Mint without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      err <- expectError $ withSender genesisAddress1 $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call Mint for nonReceiver address" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] []
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      err <- expectError $
        withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lExpectCustomError_ #assertionFailure err
  , testCase "Call Mint when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $
        withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lExpectCustomError_ #tokenOperationsArePaused err
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
      lExpectViewConsumerStorage consumer [100]
  , testCase "Successful Approve without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lExpectViewConsumerStorage consumer [100]
  , testCase "Approve for nonReceiver spender fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress1]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      err <- expectError $
        withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lExpectCustomError_ #assertionFailure err
  , testCase "Approve for nonReceiver sender fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      err <- expectError $
        withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lCallDef h $ GetAllowance
        (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lExpectCustomError_ #assertionFailure err
  , testCase "Call Approve when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $
        withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      lExpectCustomError_ #tokenOperationsArePaused err
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
      lExpectViewConsumerStorage consumer [30, 70]
  , testCase "Successful Transfer without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $
        Transfer (#from .! genesisAddress1, #to .! ownerAddress, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      lExpectViewConsumerStorage consumer [70, 30]
  , testCase "Transfer when safelist doesn't allow required pair fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      err <- expectError $
        withSender ownerAddress $
        lCallDef h $ Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #assertionFailure err
  , testCase "Transfer of more tokens than we have fails" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 150)
      lExpectCustomError #notEnoughBalance (#required .! 150, #present .! 100) err
  , testCase "Successful transfer of other's tokens" $
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
      lExpectViewConsumerStorage consumer [70, 30]
  , testCase "Transfer of more tokens than approved fails" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 70)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Transfer (#from .! genesisAddress1, #to .! ownerAddress, #value .! 100)
      lExpectCustomError #notEnoughAllowance (#required .! 100, #present .! 70) err
  , testCase "Transfer fails when token is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #tokenOperationsArePaused err
  , testCase "Transfer fails when token is not transferable" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetTransferable $ #value .! False)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Transfer (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #nonTransferable err
  ]

test_seize :: TestTree
test_seize = testGroup "Test Seize entrypoint"
 [ testCase "Successful Seize" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [(ownerAddress, genesisAddress1)] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      lExpectViewConsumerStorage consumer [30, 70]
  , testCase "Successful Seize without safelist" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      lExpectViewConsumerStorage consumer [30, 70]
  , testCase "Seize fails without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! genesisAddress1, #value .! 100)
      err <- expectError $ withSender genesisAddress1 $
        lCallDef h $ Seize (#from .! genesisAddress1, #to .! ownerAddress, #value .! 70)
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Seize when safelist doesn't allow required pair fails" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [ownerAddress]
      h <- originateHoldings ownerAddress $ Just $ toAddress sl
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #assertionFailure err
  , testCase "Seize more tokens that have" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 150)
      lExpectCustomError #notEnoughBalance (#required .! 150, #present .! 100) err
  , testCase "Seize doesn't consume allowance" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $
        Mint (#to .! genesisAddress1, #value .! 100)
      withSender genesisAddress1 $
        lCallDef h $ Approve (#spender .! ownerAddress, #value .! 100)
      withSender ownerAddress $
        lCallDef h $
        Seize (#from .! genesisAddress1, #to .! ownerAddress, #value .! 70)
      lCallDef h $ GetAllowance (mkView (#owner .! genesisAddress1, #spender .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! ownerAddress) consumer)
      lCallDef h $ GetBalance (mkView (#owner .! genesisAddress1) consumer)
      lExpectViewConsumerStorage consumer [100, 70, 30]
  , testCase "Seize fails when token is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #tokenOperationsArePaused err
  , testCase "Seize fails when token is not transferable" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetTransferable $ #value .! False)
      err <- expectError $ withSender ownerAddress $
        lCallDef h $ Seize (#from .! ownerAddress, #to .! genesisAddress1, #value .! 70)
      lExpectCustomError_ #nonTransferable err
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
      lExpectViewConsumerStorage consumer [70, 50, 0, 0]
  , testCase "Burn more tokens that have" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      err <- expectError $
        withSender ownerAddress $ lCallDef h $ Burn (#from .! ownerAddress, #value .! 150)
      lExpectCustomError #notEnoughBalance (#required .! 150, #present .! 100) err
  , testCase "Call Burn without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      err <- expectError $ withSender genesisAddress1 $ lCallDef h $
        Burn (#from .! ownerAddress, #value .! 50)
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call BurnAll without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      err <- expectError $ withSender genesisAddress1 $ lCallDef h $ BurnAll ()
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call Burn when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $ lCallDef h $
        Burn (#from .! ownerAddress, #value .! 50)
      lExpectCustomError_ #tokenOperationsArePaused err
  , testCase "Call BurnAll when contract is paused" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      withSender ownerAddress $ lCallDef h (SetPause $ #value .! True)
      err <- expectError $ withSender ownerAddress $ lCallDef h $ BurnAll ()
      lExpectCustomError_ #tokenOperationsArePaused err
  , testCase "BurnAll nullifies totalSupply" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef h $ Mint (#to .! ownerAddress, #value .! 100)
      lCallDef h $ GetTotalSupply (mkView () consumer)
      withSender ownerAddress $ lCallDef h $ BurnAll ()
      lCallDef h $ GetTotalSupply (mkView () consumer)
      lExpectViewConsumerStorage consumer [100, 0]
  ]

test_setPauseAndSetTransferable :: TestTree
test_setPauseAndSetTransferable = testGroup "Test SetPause and SetTransferable entrypoints"
  [ testCase "Call SetPause without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      err <- expectError $ withSender genesisAddress1 $ lCallDef h (SetPause $ #value .! True)
      lExpectCustomError_ #senderIsNotAdmin err
  , testCase "Call SetPause without admin rights" $
    integrationalTestExpectation $ do
      h <- originateHoldings ownerAddress $ Nothing
      err <- expectError $ withSender genesisAddress1 $ lCallDef h (SetTransferable $ #value .! True)
      lExpectCustomError_ #senderIsNotAdmin err
  ]

unit_FA1'2_is_implemented :: Assertion
unit_FA1'2_is_implemented =
  expectContractEntrypoints @AL.Parameter holdingsContract

test_documentation :: [TestTree]
test_documentation = runDocTests testLorentzDoc holdingsContract

test_approvableLedger :: IO TestTree
test_approvableLedger =
  testSpec "Holdings contract without safelist ledger tests" $
  approvableLedgerSpec originateHoldingsWithAlSettings
