-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
-- | This module contains Lorentz entrypoints of ManagedLedger contract
-- wrapped to produce Indigo code.
module Indigo.Contracts.Holdings.LorentzBindings
  ( approve
  , burn
  , ensureNotPaused
  , getAllowance
  , getBalance
  , getTotalSupply
  , mint
  , setPause
  , transfer

  -- Helpers used in seize entrypoint
  , creditTo
  , debitFrom
  ) where

import Indigo
import qualified Lorentz as L

import Indigo.Backend (fromLorentzFun2)

import qualified Lorentz.Contracts.ManagedLedger.Impl as ML
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as ML

import Indigo.Contracts.Holdings.Types

ensureNotPaused :: HasStorage Storage => IndigoProcedure
ensureNotPaused = liftIndigoState $ toSIS $
  unaryOpFlat (storageVar @Storage) (ML.ensureNotPaused # L.drop)

debitFrom :: HasStorage Storage => Var ML.TransferParams -> IndigoProcedure
debitFrom tp = do
  newStorage <- liftIndigoState $ toSIS $
    (fromLorentzFun2 $ L.framed $ ML.debitFrom # L.drop)
    tp (storageVar @Storage)
  setVar (storageVar @Storage) newStorage

creditTo :: HasStorage Storage => Var ML.TransferParams -> IndigoProcedure
creditTo tp = do
  newStorage <- liftIndigoState $ toSIS $
    (fromLorentzFun2 $ L.framed $ ML.creditTo # L.drop)
    tp (storageVar @Storage)
  setVar (storageVar @Storage) newStorage

-- | Wrap lorentz entrypoint to produce Ingido code.
-- Entrypoint produces new Storage and Ops, so the implicit Indigo variables
-- for them are updated as well.
lorentzEntrypointToIndigo
  :: (a :~> param, HasStorage Storage, HasSideEffects)
  => L.Entrypoint param Storage
  -> a -> IndigoM ()
lorentzEntrypointToIndigo entrypointCode entrypointParam = do
  entrypointRes <- liftIndigoState $ toSIS $
    (fromLorentzFun2 $ L.framed entrypointCode) entrypointParam (storageVar @Storage)
  let newStorage = snd entrypointRes
      newOps = fst entrypointRes
  -- Propagate storage changes
  setVar (storageVar @Storage) newStorage
  -- Add new chain operations
  forEach newOps $ \op -> do
    setVar operationsVar $ op .: operationsVar

transfer
  :: (a :~> ML.TransferParams, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
transfer = lorentzEntrypointToIndigo ML.transfer

approve
  :: (a :~> ML.ApproveParams, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
approve = lorentzEntrypointToIndigo ML.approve

getAllowance
  :: (a :~> View ML.GetAllowanceParams Natural, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
getAllowance = lorentzEntrypointToIndigo ML.getAllowance

getBalance
  :: (a :~> View ML.GetBalanceParams Natural, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
getBalance = lorentzEntrypointToIndigo ML.getBalance

getTotalSupply
  :: (a :~> View () Natural, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
getTotalSupply = lorentzEntrypointToIndigo ML.getTotalSupply

mint
  :: (a :~> ML.MintParams, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
mint = lorentzEntrypointToIndigo ML.mint

burn
  :: (a :~> ML.BurnParams, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
burn = lorentzEntrypointToIndigo ML.burn

setPause
  :: (a :~> Bool, HasStorage Storage, HasSideEffects)
  => a -> IndigoM ()
setPause = lorentzEntrypointToIndigo ML.setPause
