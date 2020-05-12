-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Dummy implementation for Safelist contract that complies with the specification
module Indigo.Contracts.Safelist
  ( Parameter (..)
  , Storage (..)
  , mkStorage
  , safelistContract
  ) where

import Indigo

import Data.Set (fromList)

import Indigo.Contracts.Common.Error ()
import Lorentz.Run (Contract)

data Storage = Storage
  { sTransfers :: [(Address, Address)]
  , sReceivers :: Set Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

mkStorage :: [(Address, Address)] -> [Address] -> Storage
mkStorage transfers receivers = Storage
  { sTransfers = transfers
  , sReceivers = fromList receivers
  }

data Parameter
  = AssertTransfer ("from" :! Address, "to" :! Address)
  | AssertReceiver Address
  | AssertReceivers [Address]
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type instance ErrorArg "assertionFailure" = ()

instance CustomErrorHasDoc "assertionFailure" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Assertion failed."

safelistContract :: Contract Parameter Storage
safelistContract = defaultContract $ compileIndigoContract safelistIndigo

safelistIndigo
  :: (HasStorage Storage)
  => Var Parameter -> IndigoProcedure
safelistIndigo param = contractName "Dummy safelist" $ do
  entryCase (Proxy @PlainEntryPointsKind) param
    ( #cAssertTransfer //-> \transfer -> do
        let fromAddr = transfer !. #from
            toAddr = transfer !. #to
        res <- newVar False
        forEach (storage !. #sTransfers) $ \it ->
          when ((fst it ==. fromAddr) &&. (snd it ==. toAddr)) $ setVar res True
        assertCustom_ #assertionFailure res
    , #cAssertReceiver //-> \receiver -> do
        assertCustom_ #assertionFailure $ (storage !. #sReceivers) #? receiver
    , #cAssertReceivers //-> \receivers -> do
        forEach receivers $ \receiver -> do
          assertCustom_ #assertionFailure $ (storage !. #sReceivers) #? receiver
    )

storage :: HasStorage Storage => Var Storage
storage = storageVar
