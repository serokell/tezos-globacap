-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Nettest.Holdings
  ( nettestScenario
  ) where

import Lorentz.Address
import Morley.Nettest
import Util.Named

import Indigo.Contracts.Holdings
import qualified Indigo.Contracts.Safelist as SL

nettestScenario
  :: forall m. Monad m
  => NettestImpl m -> m ()
nettestScenario = uncapsNettest $ do
  comment "Registering addresses"
  ownerAddr :: Address <- resolveNettestAddr
  adminAddr :: Address <- newAddress "admin"
  someGuyAddr :: Address <- newAddress "someGuy"
  senderAddr :: Address <- newAddress "sender"
  receiver :: Address <- newAddress "receiver"
  fakeSender :: Address <- newAddress "fakeSender"
  holdingsAddr :: Address <- originateSimple "Holdings"
    (mkStorage ownerAddr Nothing dummyMeta) holdingsContract
  safelistAddr :: Address <- originateSimple "DummySafelist"
    (SL.mkStorage [(senderAddr, receiver)] [senderAddr, receiver, adminAddr])
    SL.safelistContract
  let
    owner, admin, someGuy, sender :: AddrOrAlias
    owner = AddrResolved ownerAddr
    admin = AddrResolved adminAddr
    someGuy = AddrResolved someGuyAddr
    sender = AddrResolved senderAddr

    holdings :: AddrOrAlias
    holdings = AddrResolved holdingsAddr

  comment "Holdings nettest scenario"
  comment "Test admin rights rotation"
  callFrom owner holdings (ep "transferAdminRights") adminAddr
  expectFailure (callFrom someGuy holdings (ep "acceptAdminRights") ())
    NettestFailedWith
  callFrom admin holdings (ep "acceptAdminRights") ()
  expectFailure (callFrom someGuy holdings (ep "transferAdminRights") someGuyAddr)
    NettestFailedWith
  comment "Test token actions: mint, approve, transfer, burn"
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
    (#spender .! adminAddr, #value .! (500 :: Natural))
  callFrom admin holdings (ep "transfer")
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
