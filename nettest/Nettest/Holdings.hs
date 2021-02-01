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
      { tdTo = admin
      , tdAmount = toMutez 1500000
      , tdEntrypoint = DefEpName
      , tdParameter = ()
      }
  withSender owner $ transfer td

  comment "Holdings nettest scenario"
  comment "Test admin rights rotation"
  withSender owner $
    call holdings (Call @"TransferAdminRights") (#newAdmin .! adminAddr)
  (withSender someGuy $
    call holdings (Call @"AcceptAdminRights") ()) `expectFailure`
    NettestFailedWithError (CustomError #senderIsNotNewAdmin ())
  withSender admin $ call holdings (Call @"AcceptAdminRights") ()
  (withSender someGuy $ call holdings (Call @"TransferAdminRights") (#newAdmin .! someGuyAddr)) `expectFailure`
    NettestFailedWithError (CustomError #senderIsNotOwner ())
  comment "Test token actions: mint, approve, transfer, seize, burn"
  withSender admin $ call holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  comment "Now with safelist"
  withSender owner $ call holdings (Call @"SetSafelistAddress")
    (#newMbSafelistAddress .! Just (toAddress safelistAddr))
  (withSender admin $ call holdings (Call @"Mint")
    (#to .! fakeSender, #value .! (100500 :: Natural))) `expectFailure`
    NettestFailedWithError (CustomError #assertionFailure ())
  withSender sender $ call holdings (Call @"Approve")
    (#spender .! adminAddr, #value .! (200 :: Natural))
  (withSender admin $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))) `expectFailure`
    NettestFailedWithError
    (CustomError #notEnoughAllowance (#required .! 300, #present .! 200))
  withSender sender $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  withSender admin $ call holdings (Call @"Seize")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  (withSender someGuy $ call holdings (Call @"Burn")
    (#from .! senderAddr, #value .! (100000 :: Natural))) `expectFailure`
    NettestFailedWithError
    (CustomError #senderIsNotAdmin ())
  withSender admin $ call holdings (Call @"Burn")
    (#from .! senderAddr, #value .! (90000 :: Natural))
  (withSender admin $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))) `expectFailure`
    NettestFailedWithError
    (CustomError #notEnoughAllowance (#required .! 300, #present .! 200))
  withSender admin $ call holdings (Call @"BurnAll") ()
  comment "Disable transfers"
  withSender admin $ call holdings (Call @"SetTransferable") (#value .! False)
  withSender admin $ call holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  (withSender admin $ call holdings (Call @"Transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))) `expectFailure`
    NettestFailedWithError (CustomError #nonTransferable ())
  comment "Pause Holdings"
  withSender admin $ call holdings (Call @"SetPause") (#value .! True)
  comment "Unset safelist"
  withSender owner $ call holdings (Call @"SetSafelistAddress")
    (#newMbSafelistAddress .! Nothing)
  (withSender admin $ call holdings (Call @"Mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))) `expectFailure`
    NettestFailedWithError (CustomError #tokenOperationsArePaused ())
  comment "Unpause Holdings"
  withSender admin $ call holdings (Call @"SetPause") (#value .! False)
  comment "Mint works for nonReciever"
  withSender admin $ call holdings (Call @"Mint")
    (#to .! fakeSender, #value .! (100500 :: Natural))
