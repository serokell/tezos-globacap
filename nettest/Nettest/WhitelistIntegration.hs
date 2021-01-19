-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Nettest.WhitelistIntegration
  ( whitelistScenario
  ) where

import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set (fromList)

import Lorentz (EntrypointRef(..), mt)
import Lorentz.Address
import Michelson.Typed.Convert (untypeValue)
import Michelson.Typed.Haskell.Value (BigMap(..), IsoValue(..))
import qualified Michelson.Untyped as U
import Morley.Nettest
import Util.Named

import Indigo.Contracts.Holdings

type WhitelistId = Natural

data WhitelistStorage = WhitelistStorage
  { wsIssuer :: Address
  , wsUsers :: BigMap Address WhitelistId
  , wsWhitelists :: BigMap WhitelistId (Bool, Set WhitelistId)
  , wsAdmin :: Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

mkWhitelistStorage
  :: Address -> [(Address, WhitelistId)] -> [(WhitelistId, (Bool, Set WhitelistId))]
  -> WhitelistStorage
mkWhitelistStorage issuer users whitelists =
  WhitelistStorage
  { wsIssuer = issuer
  , wsUsers = BigMap $ Map.fromList users
  , wsWhitelists = BigMap $ Map.fromList whitelists
  , wsAdmin = issuer
  }

whitelistScenario :: U.Contract -> NettestScenario m
whitelistScenario whitelistContract = uncapsNettest $ do
  comment "Registering addresses"
  ownerAddr :: Address <- resolveNettestAddress
  issuerAddr :: Address <- newAddress "issuer"
  senderAddr :: Address <- newAddress "sender"
  receiver :: Address <- newAddress "receiver"
  fakeSender :: Address <- newAddress "fakeSender"
  whitelist <- originateUntypedSimple "Whitelist"
    ( untypeValue $ toVal $ mkWhitelistStorage issuerAddr
      [ (ownerAddr, 0)
      , (senderAddr, 0)
      , (receiver, 1)
      , (fakeSender, 2)
      ]
      [ (0, (True, Set.fromList [1]))
      , (1, (True, mempty))
      , (2, (False, mempty))
      ]
    ) whitelistContract
  holdings :: TAddress Parameter <- originateSimple "Holdings"
    (mkStorage ownerAddr ownerAddr Nothing mempty 0 dummyMeta) holdingsContract
  let
    owner, sender :: AddressOrAlias
    owner = AddressResolved ownerAddr
    sender = AddressResolved senderAddr

  comment "Scenario with Whitelist"
  comment "Set whitelist as a safelist contract"
  withSender owner $ call holdings (Call @"SetSafelistAddress") (#newMbSafelistAddress .! Just whitelist)
  comment "Proper mint, approve, transfer"
  withSender owner $ call holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  withSender sender $ call holdings (Call @"Approve")
    (#spender .! ownerAddr, #value .! (500 :: Natural))
  withSender owner $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  comment "Unacceptable mint"
  (withSender owner $ call holdings (Call @"Mint")
    (#to .! fakeSender, #value .! (100500 :: Natural))) `expectFailure`
    NettestFailedWith whitelist [mt|outbound restricted|]
  comment "Unacceptable approve"
  (withSender sender $ call holdings (Call @"Approve")
    (#spender .! fakeSender, #value .! (10 :: Natural))) `expectFailure`
    NettestFailedWith whitelist [mt|outbound restricted|]
  comment "Unacceptable transfer"
  (withSender owner $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! fakeSender, #value .! (200 :: Natural))) `expectFailure`
    NettestFailedWith whitelist [mt|outbound not whitelisted|]
