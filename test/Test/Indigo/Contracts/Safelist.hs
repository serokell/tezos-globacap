module Test.Indigo.Contracts.Safelist
  ( test_SafelistSets
  , test_ensureSafelistConstraints
  , test_documentation
  ) where

import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz (Address, Label, TAddress(..), View, ToAddress, mkView, mt)
import qualified Lorentz as L
import Lorentz.Test
import Michelson.Runtime.GState (genesisAddress, genesisAddress1, genesisAddress2)
import Tezos.Core (toMutez)
import Util.Named ((.!))

import Indigo.Contracts.Safelist

originateSafelist :: Address -> IntegrationalScenarioM (TAddress Parameter)
originateSafelist owner =
  lOriginate safelistContract "safelist" (mkStorage owner) (toMutez 0)

ownerAddress :: Address
ownerAddress = genesisAddress

test_SafelistSets :: TestTree
test_SafelistSets = testGroup "Tests to check sets related entrypoints"
  [ genericSetTestTree adminsHandler
  , genericSetTestTree whitelistHandler
  , genericSetTestTree blacklistHandler
  ]

expectStorageUpdate
  :: (ToAddress addr)
  => addr -> (Storage -> Bool) -> Text -> SuccessValidator
expectStorageUpdate contractAddr validateStorage msg =
  lExpectStorageUpdate contractAddr
    (\st -> bool
      (Left $ CustomValidationError msg) (Right ()) $ validateStorage st
    )

data SetHandler errTag = SetHandler
  { shAdder :: Address -> Parameter
  , shRemover :: Address -> Parameter
  , shGetter :: View Address Bool -> Parameter
  , shErrorTag :: Label errTag
  , shField :: Storage -> Set Address
  , shSetName :: String
  }

adminsHandler :: SetHandler "senderIsNotOwner"
adminsHandler = SetHandler
  { shAdder = AddAdmin
  , shRemover = RemoveAdmin
  , shGetter = IsAdmin
  , shErrorTag = #senderIsNotOwner
  , shField = sAdmins
  , shSetName = "admins"
  }

whitelistHandler :: SetHandler "senderIsNotAdmin"
whitelistHandler = SetHandler
  { shAdder = AddToWhitelist
  , shRemover = RemoveFromWhitelist
  , shGetter = IsWhitelisted
  , shErrorTag = #senderIsNotAdmin
  , shField = sWhitelist
  , shSetName = "whitelist"
  }

blacklistHandler :: SetHandler "senderIsNotAdmin"
blacklistHandler = SetHandler
  { shAdder = AddToBlacklist
  , shRemover = RemoveFromBlacklist
  , shGetter = IsBlacklisted
  , shErrorTag = #senderIsNotAdmin
  , shField = sBlacklist
  , shSetName = "blacklist"
  }

genericSetTestTree
  :: (L.IsError (L.CustomError errTag), L.ErrorArg errTag ~ ())
  => SetHandler errTag -> TestTree
genericSetTestTree SetHandler{..} =
  testGroup ("Test " <> shSetName <> " related entrypoints")
  [ testGroup "Test adder" $
    [ testCase "Succesfully call adder" $
      integrationalTestExpectation $ do
        sl <- originateSafelist ownerAddress
        withSender ownerAddress $ lCallDef sl (shAdder genesisAddress1)
        validate . Right $
          expectStorageUpdate sl (\st -> Set.member genesisAddress1 $ shField st)
          "New entity not found"
    , testCase "Call adder without sufficient rights" $
    integrationalTestExpectation $ do
      sl <- originateSafelist ownerAddress
      withSender genesisAddress1 $ lCallDef sl (shAdder genesisAddress1)
      validate . Left $ lExpectCustomError_ shErrorTag
    ]
  , testGroup "Test remover" $
    [ testCase "Succesfully call remover" $
      integrationalTestExpectation $ do
        sl <- originateSafelist ownerAddress
        withSender ownerAddress $ lCallDef sl (shAdder genesisAddress1)
        withSender ownerAddress $ lCallDef sl (shRemover genesisAddress1)
        validate . Right $
          expectStorageUpdate sl (\st -> not (Set.member genesisAddress1 $ shField st))
          "New entity still found"
    , testCase "Call remover without sufficient rights" $
      integrationalTestExpectation $ do
        sl <- originateSafelist ownerAddress
        withSender ownerAddress $ lCallDef sl (shAdder genesisAddress1)
        withSender genesisAddress1 $ lCallDef sl (shRemover genesisAddress1)
        validate . Left $ lExpectCustomError_ shErrorTag
    ]
  , testCase "Test getter" $
    integrationalTestExpectation $ do
      sl <- originateSafelist ownerAddress
      consumer <- lOriginateEmpty contractConsumer "consumer"
      withSender ownerAddress $ lCallDef sl $ shAdder genesisAddress1
      lCallDef sl $ shGetter (mkView genesisAddress1 consumer)
      lCallDef sl $ shGetter (mkView genesisAddress2 consumer)
      validate . Right $
        lExpectViewConsumerStorage consumer [True, False]
  ]

test_ensureSafelistConstraints :: TestTree
test_ensureSafelistConstraints = testGroup "Test EnsureSafelistConstraints entrypoint"
  [ testCase "Does nothing when all constraints are satisfied" $
    integrationalTestExpectation $ do
      sl <- originateSafelist ownerAddress
      withSender ownerAddress $ lCallDef sl $ AddToWhitelist genesisAddress1
      withSender ownerAddress $ lCallDef sl $ AddToWhitelist genesisAddress2
      lCallDef sl $ EnsureSafelistConstraints
        ( #isWhitelisted .! [genesisAddress1, genesisAddress2]
        , #isNonBlacklisted .! [genesisAddress1, genesisAddress2]
        )
      validate . Right $ expectNoStorageUpdates
  , testCase "Fails on first unsatisfied constraint ver 1" $
    integrationalTestExpectation $ do
      sl <- originateSafelist ownerAddress
      withSender ownerAddress $ lCallDef sl $ AddToWhitelist genesisAddress1
      lCallDef sl $ EnsureSafelistConstraints
        ( #isWhitelisted .! [genesisAddress1, genesisAddress2]
        , #isNonBlacklisted .! [genesisAddress1, genesisAddress2]
        )
      validate . Left $
        lExpectCustomError #constraintUnsatisfied (genesisAddress2, [mt|nonWhitelisted|])
  , testCase "Fails on first unsatisfied constraint ver 2" $
    integrationalTestExpectation $ do
      sl <- originateSafelist ownerAddress
      withSender ownerAddress $ lCallDef sl $ AddToWhitelist genesisAddress1
      withSender ownerAddress $ lCallDef sl $ AddToWhitelist genesisAddress2
      withSender ownerAddress $ lCallDef sl $ AddToBlacklist genesisAddress2
      lCallDef sl $ EnsureSafelistConstraints
        ( #isWhitelisted .! [genesisAddress1, genesisAddress2]
        , #isNonBlacklisted .! [genesisAddress1, genesisAddress2]
        )
      validate . Left $
        lExpectCustomError #constraintUnsatisfied (genesisAddress2, [mt|blacklisted|])
  ]

test_documentation :: [TestTree]
test_documentation = runDocTests testLorentzDoc safelistDoc
