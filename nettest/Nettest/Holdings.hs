-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Nettest.Holdings
  ( nettestScenario
  ) where

import Lorentz (CustomError(..), EntrypointRef(..))
import Lorentz.Address
import Michelson.Untyped.Entrypoints
import Morley.Nettest
import Util.Named
import Tezos.Core (toMutez)

import Indigo.Contracts.Holdings
import qualified Indigo.Contracts.Safelist as SL

nettestScenario
  :: forall m. Monad m
  => NettestImpl m -> m ()
nettestScenario = uncapsNettest $ do
  comment "Registering addresses"
  ownerAddr :: Address <- resolveNettestAddress
  adminAddr :: Address <- newAddress "admin"
  someGuyAddr :: Address <- newAddress "someGuy"
  senderAddr :: Address <- newAddress "sender"
  receiver :: Address <- newAddress "receiver"
  fakeSender :: Address <- newAddress "fakeSender"
  (holdings :: TAddress Parameter) <- originateSimple "Holdings"
    (mkStorage ownerAddr ownerAddr Nothing mempty 0 dummyMeta) holdingsContract
  (safelistAddr :: TAddress SL.Parameter) <- originateSimple "DummySafelist"
    (SL.mkStorage [(senderAddr, receiver)] [senderAddr, receiver, adminAddr])
    SL.safelistContract
  let
    owner, admin, someGuy, sender :: AddressOrAlias
    owner = AddressResolved ownerAddr
    admin = AddressResolved adminAddr
    someGuy = AddressResolved someGuyAddr
    sender = AddressResolved senderAddr

    -- We transfer additional mutez to admin so that he doesn't run out of it.
    -- Probably can be removed after https://gitlab.com/morley-framework/morley/-/issues/139
    -- is resolved. Currently we spend 10 times more fee than needed.
    td = TransferData
      { tdFrom = owner
      , tdTo = admin
      , tdAmount = toMutez 1500000
      , tdEntrypoint = DefEpName
      , tdParameter = ()
      }
  transfer td

  comment "Holdings nettest scenario"
  comment "Test admin rights rotation"
  callFrom owner holdings (Call @"TransferAdminRights") (#newAdmin .! adminAddr)
  callFrom someGuy holdings (Call @"AcceptAdminRights") () `expectFailure`
    NettestFailedWithError (CustomError #senderIsNotNewAdmin ())
  callFrom admin holdings (Call @"AcceptAdminRights") ()
  callFrom someGuy holdings (Call @"TransferAdminRights") (#newAdmin .! someGuyAddr) `expectFailure`
    NettestFailedWithError (CustomError #senderIsNotOwner ())
  comment "Test token actions: mint, approve, transfer, seize, burn"
  callFrom admin holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  comment "Now with safelist"
  callFrom owner holdings (Call @"SetSafelistAddress")
    (#newMbSafelistAddress .! Just (toAddress safelistAddr))
  callFrom admin holdings (Call @"Mint")
    (#to .! fakeSender, #value .! (100500 :: Natural)) `expectFailure`
    NettestFailedWithError (CustomError #assertionFailure ())
  callFrom sender holdings (Call @"Approve")
    (#spender .! adminAddr, #value .! (200 :: Natural))
  callFrom admin holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural)) `expectFailure`
    NettestFailedWithError
    (CustomError #notEnoughAllowance (#required .! 300, #present .! 200))
  callFrom sender holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  callFrom admin holdings (Call @"Seize")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  callFrom someGuy holdings (Call @"Burn")
    (#from .! senderAddr, #value .! (100000 :: Natural)) `expectFailure`
    NettestFailedWithError
    (CustomError #senderIsNotAdmin ())
  callFrom admin holdings (Call @"Burn")
    (#from .! senderAddr, #value .! (90000 :: Natural))
  callFrom admin holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural)) `expectFailure`
    NettestFailedWithError
    (CustomError #notEnoughAllowance (#required .! 300, #present .! 200))
  callFrom admin holdings (Call @"BurnAll") ()
  comment "Disable transfers"
  callFrom admin holdings (Call @"SetTransferable") (#value .! False)
  callFrom admin holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  callFrom admin holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural)) `expectFailure`
    NettestFailedWithError (CustomError #nonTransferable ())
  comment "Pause Holdings"
  callFrom admin holdings (Call @"SetPause") (#value .! True)
  comment "Unset safelist"
  callFrom owner holdings (Call @"SetSafelistAddress")
    (#newMbSafelistAddress .! Nothing)
  callFrom admin holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural)) `expectFailure`
    NettestFailedWithError (CustomError #tokenOperationsArePaused ())
  comment "Unpause Holdings"
  callFrom admin holdings (Call @"SetPause") (#value .! False)
  comment "Mint works for nonReciever"
  callFrom admin holdings (Call @"Mint")
    (#to .! fakeSender, #value .! (100500 :: Natural))
