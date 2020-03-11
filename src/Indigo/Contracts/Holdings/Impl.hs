-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Indigo.Contracts.Holdings.Impl
  ( holdingsContract
  ) where

import Indigo

import Lorentz.EntryPoints (EntryPointRef(..), eprName)

import Indigo.Contracts.Common.Error ()
import Indigo.Contracts.Holdings.Error ()
import Indigo.Contracts.Holdings.LorentzBindings
import Indigo.Contracts.Holdings.Types

storage :: HasStorage Storage => Var Storage
storage = storageVar

ensureSenderIsOwner :: HasStorage Storage => IndigoProcedure
ensureSenderIsOwner = do
  let ownerAddr = storage !. #fields !. #owner
  assertCustom_ #senderIsNotOwner $ ownerAddr ==. sender

ensureSenderIsAdmin :: HasStorage Storage => IndigoProcedure
ensureSenderIsAdmin = do
  let admin = storage !. #fields !. #admin
  assertCustom_ #senderIsNotAdmin $ admin ==. sender

ensureIsTransferable :: HasStorage Storage => IndigoProcedure
ensureIsTransferable = do
  let transferable = storage !. #fields !. #transferable
  assertCustom_ #nonTransferable transferable

holdingsContract :: ContractCode Parameter Storage
holdingsContract = optimizeLorentz $ compileIndigoContract holdingsIndigo

holdingsIndigo
  :: (HasStorage Storage, HasSideEffects)
  => Var Parameter -> IndigoProcedure
holdingsIndigo param = do
  entryCase (Proxy @PlainEntryPointsKind) param
    ( #cSetName //-> \newName -> do
        ensureSenderIsAdmin
        ensureNotPaused
        setStorageField @Storage #tokenName $ UnName #newName newName
    , #cSetSymbol //-> \newSymbol -> do
        ensureSenderIsAdmin
        ensureNotPaused
        setStorageField @Storage #tokenSymbol $ UnName #newSymbol newSymbol
    , #cSetSafelistAddress //-> \newMbSafelistAddrNamed -> do
        ensureSenderIsOwner
        let newMbSafelistAddr = UnName #newMbSafelistAddress newMbSafelistAddrNamed
        setStorageField @Storage #mbSafelistAddress newMbSafelistAddr
        whenJust newMbSafelistAddr
          (\addr -> do
              -- Check that new safelist has required entrypoints
              whenNothing
                (contractCallingUnsafe @("from" :! Address, "to" :! Address)
                 (eprName $ Call @"assertTransfer") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertTransfer|])
              whenNothing
                (contractCallingUnsafe @Address
                 (eprName $ Call @"assertReceiver") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertReceiver|])
              whenNothing
                (contractCallingUnsafe @[Address]
                 (eprName $ Call @"assertReceivers") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertReceivers|])
          )
    , #cTransferAdminRights //-> \newAdmin -> do
        ensureSenderIsOwner
        ensureNotPaused
        setStorageField @Storage #mbNewAdmin $ some $ UnName #newAdmin newAdmin
    , #cAcceptAdminRights //-> \_unit -> do
        ifJust (storage !. #fields !. #mbNewAdmin)
          (\newAdmin ->
              if newAdmin /=. sender
                then () <$ failCustom_ #senderIsNotNewAdmin
                else do
                setStorageField @Storage #admin newAdmin
                setStorageField @Storage #mbNewAdmin none
          )
          (() <$ failCustom_ #notInTransferAdminRightsMode)
    , #cIsAdmin //-> \v -> do
        project v $ \addr -> return $ (storage !. #fields !. #admin) ==. addr
    , #cTransfer //-> \tParams -> do
        ensureSenderIsAdmin
        -- pause check is already done in Lorentz code
        ensureIsTransferable
        let from = tParams !. #from
            to = tParams !. #to
        callAssertTransfer $ pair (Name #from from) (Name #to to)
        transfer tParams
    , #cApprove //-> \aParams -> do
        -- pause check is already done in Lorentz code
        let spender = aParams !. #spender
            receivers = spender .: sender .: nil
        callAssertReceivers receivers
        approve aParams
    , #cGetAllowance //-> getAllowance
    , #cGetBalance //-> getBalance
    , #cGetTotalSupply //-> getTotalSupply
    , #cMint //-> \mParams -> do
        ensureNotPaused
        -- admin check is already done in Lorentz code
        let to = mParams !. #to
        callAssertReceiver to
        mint mParams
    , #cBurn //-> \bParams -> do
        ensureNotPaused
        -- admin check is already done in Lorentz code
        burn bParams
    , #cBurnAll //-> \_ -> do
        ensureSenderIsAdmin
        ensureNotPaused
        setField_ storage #ledger emptyBigMap
    , #cSetPause //-> \pausedNamed -> do
        -- admin check is already done in Lorentz code
        let paused = UnName #value pausedNamed
        setPause paused
    , #cSetTransferable //-> \transferable -> do
        ensureSenderIsAdmin
        setStorageField @Storage #transferable $ UnName #value transferable
    , #cGetTokenMeta //-> \v -> do
        project v $ \_ -> return (storage !. #fields !. #tokenMeta)
    )

callAssertTransfer
  :: (HasStorage Storage, HasSideEffects, a :~> ("from" :! Address, "to" :! Address))
  => a -> IndigoProcedure
callAssertTransfer a = do
  whenJust (storage !. #fields !. #mbSafelistAddress)
    (\addr -> do
        ifJust (contractCallingUnsafe @("from" :! Address, "to" :! Address)
                 (eprName $ Call @"assertTransfer") addr
               )
          (\cAddr -> transferTokens a (toMutez 0) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertTransfer|])
    )

callAssertReceiver
  :: (HasStorage Storage, HasSideEffects, a :~> Address)
  => a -> IndigoProcedure
callAssertReceiver a = do
  whenJust (storage !. #fields !. #mbSafelistAddress)
    (\addr -> do
        ifJust (contractCallingUnsafe @Address
                 (eprName $ Call @"assertReceiver") addr
               )
          (\cAddr -> transferTokens a (toMutez 0) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertReceiver|])
    )

callAssertReceivers
  :: (HasStorage Storage, HasSideEffects, a :~> [Address])
  => a -> IndigoProcedure
callAssertReceivers a = do
  whenJust (storage !. #fields !. #mbSafelistAddress)
    (\addr -> do
        ifJust (contractCallingUnsafe @[Address]
                 (eprName $ Call @"assertReceivers") addr
               )
          (\cAddr -> transferTokens a (toMutez 0) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertReceivers|])
    )
