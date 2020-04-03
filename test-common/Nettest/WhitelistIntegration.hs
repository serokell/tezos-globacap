-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Nettest.WhitelistIntegration
  ( whitelistScenario
  ) where

import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set (fromList)

import Lorentz hiding (sender)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Typed.Haskell.Value (IsoValue(..))
import qualified Michelson.Untyped as U
import Morley.Nettest
import Morley.Nettest.Caps (originateUntypedSimple)
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
mkWhitelistStorage issuer users whitelists = WhitelistStorage issuer
  (BigMap $ Map.fromList users) (BigMap $ Map.fromList whitelists) issuer

whitelistScenario :: U.Contract -> NettestScenario
whitelistScenario whitelistContract = uncapsNettest $ do
  comment "Registering addresses"
  ownerAddr :: Address <- resolveNettestAddr
  senderAddr :: Address <- newAddress "sender"
  receiver :: Address <- newAddress "receiver"
  fakeSender :: Address <- newAddress "fakeSender"
  whitelist <- originateUntypedSimple "Whitelist"
    ( untypeValue $ toVal $ mkWhitelistStorage ownerAddr
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
  holdingsAddr :: Address <- originateSimple "Holdings"
    (mkStorage ownerAddr ownerAddr Nothing dummyMeta) holdingsContract
  let
    owner, sender, holdings :: AddrOrAlias
    owner = AddrResolved ownerAddr
    sender = AddrResolved senderAddr
    holdings = AddrResolved holdingsAddr

  comment "Scenario with Whitelist"
  comment "Set whitelist as a safelist contract"
  callFrom owner holdings (ep "setSafelistAddress") (Just whitelist)
  comment "Proper mint, approve, transfer"
  callFrom owner holdings (ep "mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  callFrom sender holdings (ep "approve")
    (#spender .! ownerAddr, #value .! (500 :: Natural))
  callFrom owner holdings (ep "transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  comment "Unacceptable mint"
  expectFailure
    (callFrom owner holdings (ep "mint")
      (#to .! fakeSender, #value .! (100500 :: Natural))
    )
    NettestFailedWith
  comment "Unacceptable approve"
  expectFailure
    (callFrom sender holdings (ep "approve")
      (#spender .! fakeSender, #value .! (10 :: Natural))
    )
    NettestFailedWith
  callFrom owner holdings (ep "setPause") True
  comment "Unacceptable transfer"
  expectFailure
    (callFrom owner holdings (ep "transfer")
      (#from .! senderAddr, #to .! fakeSender, #value .! (200 :: Natural))
    )
    NettestFailedWith
  callFrom owner holdings (ep "setPause") True
