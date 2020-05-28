-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Nettest.Holdings
  ( nettestScenario
  ) where

import Lorentz.Address
import Michelson.Untyped.EntryPoints
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
  holdingsAddr :: Address <- originateSimple "Holdings"
    (mkStorage ownerAddr ownerAddr Nothing mempty 0 dummyMeta) holdingsContract
  safelistAddr :: Address <- originateSimple "DummySafelist"
    (SL.mkStorage [(senderAddr, receiver)] [senderAddr, receiver, adminAddr])
    SL.safelistContract
  let
    owner, admin, someGuy, sender :: AddressOrAlias
    owner = AddressResolved ownerAddr
    admin = AddressResolved adminAddr
    someGuy = AddressResolved someGuyAddr
    sender = AddressResolved senderAddr

    holdings :: AddressOrAlias
    holdings = AddressResolved holdingsAddr
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
  callFrom owner holdings (ep "transferAdminRights") adminAddr
  expectFailure (callFrom someGuy holdings (ep "acceptAdminRights") ())
    NettestFailedWith
  callFrom admin holdings (ep "acceptAdminRights") ()
  expectFailure (callFrom someGuy holdings (ep "transferAdminRights") someGuyAddr)
    NettestFailedWith
  comment "Test token actions: mint, approve, transfer, seize, burn"
  callFrom admin holdings (ep "mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  comment "Now with safelist"
  callFrom owner holdings (ep "setSafelistAddress") (Just safelistAddr)
  expectFailure
    (callFrom admin holdings (ep "mint")
      (#to .! fakeSender, #value .! (100500 :: Natural))
    )
    NettestFailedWith
  callFrom sender holdings (ep "approve")
    (#spender .! adminAddr, #value .! (200 :: Natural))
  expectFailure
    (callFrom admin holdings (ep "transfer")
      (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
    )
    NettestFailedWith
  callFrom sender holdings (ep "transfer")
    (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  callFrom admin holdings (ep "seize")
      (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
  expectFailure
    (callFrom someGuy holdings (ep "burn")
      (#from .! senderAddr, #value .! (100000 :: Natural))
    )
    NettestFailedWith
  callFrom admin holdings (ep "burn")
    (#from .! senderAddr, #value .! (90000 :: Natural))
  expectFailure
    (callFrom admin holdings (ep "transfer")
      (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
    )
    NettestFailedWith
  callFrom admin holdings (ep "burnAll") ()
  comment "Disable transfers"
  callFrom admin holdings (ep "setTransferable") False
  callFrom admin holdings (ep "mint")
    (#to .! senderAddr, #value .! (100500 :: Natural))
  expectFailure
    (callFrom admin holdings (ep "transfer")
      (#from .! senderAddr, #to .! receiver, #value .! (300 :: Natural))
    )
    NettestFailedWith
  comment "Pause Holdings"
  callFrom admin holdings (ep "setPause") True
  comment "Unset safelist"
  callFrom owner holdings (ep "setSafelistAddress") (Nothing :: Maybe Address)
  expectFailure
    (callFrom admin holdings (ep "mint")
      (#to .! senderAddr, #value .! (100500 :: Natural))
    ) NettestFailedWith
  comment "Unpause Holdings"
  callFrom admin holdings (ep "setPause") False
  comment "Mint works for nonReciever"
  callFrom admin holdings (ep "mint")
    (#to .! fakeSender, #value .! (100500 :: Natural))
