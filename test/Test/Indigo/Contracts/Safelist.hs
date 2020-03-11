-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Test.Indigo.Contracts.Safelist
  ( test_dummySafelist
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz (Address, TAddress(..))
import Lorentz.Test
import Michelson.Runtime.GState (genesisAddress, genesisAddress1, genesisAddress2)
import Tezos.Core (toMutez)
import Util.Named ((.!))

import Indigo.Contracts.Safelist

originateSafelist
  :: [(Address, Address)] -> [Address] -> IntegrationalScenarioM (TAddress Parameter)
originateSafelist transfers receivers =
  lOriginate safelistContract "safelist" (mkStorage transfers receivers) (toMutez 0)

test_dummySafelist :: TestTree
test_dummySafelist = testGroup "Test Dummy Safelist contract"
  [ testCase "AssertTransfer does nothing when pair is presented in transfers list" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [(genesisAddress1, genesisAddress2)] []
      lCallDef sl $ AssertTransfer (#from .! genesisAddress1, #to .! genesisAddress2)
      validate . Right $ expectAnySuccess
  , testCase "AssertTransfer fails when pair is not presented in transfers list" $
    integrationalTestExpectation $ do
      sl <- originateSafelist
        [ (genesisAddress1, genesisAddress2)
        , (genesisAddress, genesisAddress1)
        ] []
      lCallDef sl $ AssertTransfer (#from .! genesisAddress2, #to .! genesisAddress1)
      validate . Left $ lExpectCustomError_ #assertionFailure
  , testCase "AssertReceiver does nothing when address is presented in receivers set" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress1]
      lCallDef sl $ AssertReceiver genesisAddress1
      validate . Right $ expectAnySuccess
  , testCase "AssertReceiver fails when address is not presented in receivers set" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress, genesisAddress2]
      lCallDef sl $ AssertReceiver genesisAddress1
      validate . Left $ lExpectCustomError_ #assertionFailure
  , testCase "AssertReceivers does nothing when all addresses are presented in receivers set" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress1, genesisAddress2]
      lCallDef sl $ AssertReceivers [genesisAddress1, genesisAddress2]
      validate . Right $ expectAnySuccess
  , testCase "AssertReceivers fails when one address is not presented in receivers set" $
    integrationalTestExpectation $ do
      sl <- originateSafelist [] [genesisAddress, genesisAddress2]
      lCallDef sl $ AssertReceivers [genesisAddress, genesisAddress1]
      validate . Left $ lExpectCustomError_ #assertionFailure
  ]