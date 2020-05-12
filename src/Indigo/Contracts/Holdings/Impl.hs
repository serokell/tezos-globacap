-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Indigo.Contracts.Holdings.Impl
  ( holdingsContract
  , holdingsDoc
  ) where

import Indigo

import Indigo.Contracts.Common.Error ()
import Indigo.Contracts.Holdings.Error ()
import Indigo.Contracts.Holdings.LorentzBindings
import Indigo.Contracts.Holdings.Types
import Lorentz.Run (Contract(..))

storage :: HasStorage Storage => Var Storage
storage = storageVar

ensureSenderIsOwner :: HasStorage Storage => IndigoProcedure
ensureSenderIsOwner = do
  ownerAddr <- getStorageField @Storage #owner
  assertCustom_ #senderIsNotOwner $ ownerAddr ==. sender

ensureSenderIsAdmin :: HasStorage Storage => IndigoProcedure
ensureSenderIsAdmin = do
  admin <- getStorageField @Storage #admin
  assertCustom_ #senderIsNotAdmin $ admin ==. sender

ensureIsTransferable :: HasStorage Storage => IndigoProcedure
ensureIsTransferable = do
  transferable <- getStorageField @Storage #transferable
  assertCustom_ #nonTransferable transferable

holdingsContract :: Contract Parameter Storage
holdingsContract = defaultContract $ compileIndigoContract holdingsIndigo

holdingsDoc :: ContractDoc
holdingsDoc = buildLorentzDoc $ cCode holdingsContract

holdingsIndigo
  :: (HasStorage Storage, HasSideEffects)
  => Var Parameter -> IndigoProcedure
holdingsIndigo param = contractName "Holdings" $ do
  contractGeneral $ doc DGitRevisionUnknown
  docStorage @Storage
  doc $ DDescription
    "This contract is used to distribute the token, it is optionally regulated by \
    \the Safelist contract."
  entryCaseSimple param
    ( #cSetName //-> \newName -> do
        doc $ DDescription "Change token name."
        ensureSenderIsAdmin
        ensureNotPaused
        setStorageField @Storage #tokenName $ UnName #newName newName
    , #cSetSymbol //-> \newSymbol -> do
        doc $ DDescription "Change token symbol."
        ensureSenderIsAdmin
        ensureNotPaused
        setStorageField @Storage #tokenSymbol $ UnName #newSymbol newSymbol
    , #cSetSafelistAddress //-> \newMbSafelistAddrNamed -> do
        doc $ DDescription
          "Change optional Safelist contract address."
        ensureSenderIsOwner
        let newMbSafelistAddr = UnName #newMbSafelistAddress newMbSafelistAddrNamed
        setStorageField @Storage #mbSafelistAddress newMbSafelistAddr
        whenSome newMbSafelistAddr
          (\addr -> do
              -- Check that new safelist has required entrypoints
              whenNone
                (contractCallingUnsafe @("from" :! Address, "to" :! Address)
                 (eprName $ Call @"assertTransfer") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertTransfer|])
              whenNone
                (contractCallingUnsafe @Address
                 (eprName $ Call @"assertReceiver") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertReceiver|])
              whenNone
                (contractCallingUnsafe @[Address]
                 (eprName $ Call @"assertReceivers") addr
                ) (() <$ failCustom #invalidSafelistAddr [mt|assertReceivers|])
          )
    , #cTransferAdminRights //-> \newAdmin -> do
        doc $ DDescription "Transfer admin rights to the new address."
        ensureSenderIsOwner
        ensureNotPaused
        setStorageField @Storage #mbNewAdmin $ some $ UnName #newAdmin newAdmin
    , #cAcceptAdminRights //-> \_unit -> do
        doc $ DDescription "Accept admin rights by the new admin."
        mbNewAdmin <- getStorageField @Storage #mbNewAdmin
        ifSome mbNewAdmin
          (\newAdmin ->
              if newAdmin /=. sender
                then () <$ failCustom_ #senderIsNotNewAdmin
                else do
                setStorageField @Storage #admin newAdmin
                setStorageField @Storage #mbNewAdmin none
          )
          (() <$ failCustom_ #notInTransferAdminRightsMode)
    , #cIsAdmin //-> \v -> do
        doc $ DDescription
          "Check whether address is admin. Returns `True` if the address is the admin."
        project v $ \addr -> do
          admin <- getStorageField @Storage #admin
          return $ admin ==. addr
    , #cTransfer //-> \tParams -> do
        -- pause check is already done in Lorentz code
        ensureIsTransferable
        let from = tParams !. #from
            to = tParams !. #to
        callAssertTransfer $ pair (Name #from from) (Name #to to)
        transfer tParams
    , #cSeize //-> \tParams -> do
        doc $ DDescription
          "Forcibly send given amount of tokens from one address to another."
        ensureSenderIsAdmin
        ensureNotPaused
        ensureIsTransferable
        let from = tParams !. #from
            to = tParams !. #to
        callAssertTransfer $ pair (Name #from from) (Name #to to)
        debitFrom tParams
        creditTo tParams
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
        doc $ DDescription "Destroy all tokens and allowances."
        ensureSenderIsAdmin
        ensureNotPaused
        setField storage #ledger emptyBigMap
        setField storage #approvals emptyBigMap
        setStorageField @Storage #totalSupply (0 nat)
    , #cSetPause //-> \pausedNamed -> do
        -- admin check is already done in Lorentz code
        let paused = UnName #value pausedNamed
        setPause paused
    , #cSetTransferable //-> \transferable -> do
        doc $ DDescription "Change transferable flag."
        ensureSenderIsAdmin
        setStorageField @Storage #transferable $ UnName #value transferable
    , #cGetTokenMeta //-> \v -> do
        doc $ DDescription "Get token meta data: name, symbol and id."
        project v $ \_ -> getStorageField @Storage #tokenMeta
    )

callAssertTransfer
  :: (HasStorage Storage, HasSideEffects, a :~> ("from" :! Address, "to" :! Address))
  => a -> IndigoProcedure
callAssertTransfer a = do
  mbSafelistAddress <- getStorageField @Storage #mbSafelistAddress
  whenSome mbSafelistAddress
    (\addr -> do
        ifSome (contractCallingUnsafe @("from" :! Address, "to" :! Address)
                 (eprName $ Call @"assertTransfer") addr
               )
          (\cAddr -> transferTokens a (0 mutez) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertTransfer|])
    )

callAssertReceiver
  :: (HasStorage Storage, HasSideEffects, a :~> Address)
  => a -> IndigoProcedure
callAssertReceiver a = do
  mbSafelistAddress <- getStorageField @Storage #mbSafelistAddress
  whenSome mbSafelistAddress
    (\addr -> do
        ifSome (contractCallingUnsafe @Address
                 (eprName $ Call @"assertReceiver") addr
               )
          (\cAddr -> transferTokens a (0 mutez) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertReceiver|])
    )

callAssertReceivers
  :: (HasStorage Storage, HasSideEffects, a :~> [Address])
  => a -> IndigoProcedure
callAssertReceivers a = do
  mbSafelistAddress <- getStorageField @Storage #mbSafelistAddress
  whenSome mbSafelistAddress
    (\addr -> do
        ifSome (contractCallingUnsafe @[Address]
                 (eprName $ Call @"assertReceivers") addr
               )
          (\cAddr -> transferTokens a (0 mutez) cAddr)
          (() <$ failCustom #invalidSafelistAddr [mt|assertReceivers|])
    )
