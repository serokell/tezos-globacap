{-# OPTIONS_GHC -Wno-orphans #-}
module Indigo.Contracts.Safelist
  ( Parameter (..)
  , Storage (..)
  , mkStorage
  , safelistContract
  ) where

import Indigo

import Data.Set (fromList)

data Storage = Storage
  { sOwner :: Address
  , sAdmins :: Set Address
  , sWhitelist :: Set Address
  , sBlacklist :: Set Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

mkStorage :: Address -> Storage
mkStorage owner = Storage
  { sOwner = owner
  , sAdmins = fromList $ [owner]
  , sWhitelist = mempty
  , sBlacklist = mempty
  }

data Parameter
  = AddAdmin Address
  | RemoveAdmin Address
  | IsAdmin (View Address Bool)
  | AddToWhitelist Address
  | RemoveFromWhitelist Address
  | IsWhitelisted (View Address Bool)
  | AddToBlacklist Address
  | RemoveFromBlacklist Address
  | IsBlacklisted (View Address Bool)
  | EnsureSafelistConstraints EnsureSafelistConstraintsParam
  deriving stock Generic
  deriving anyclass IsoValue

type EnsureSafelistConstraintsParam = ("isWhitelisted" :! [Address], "isNonBlacklisted" :! [Address])

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type instance ErrorArg "senderIsNotOwner" = ()

instance CustomErrorHasDoc "senderIsNotOwner" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Sender is not the contract owner."

type instance ErrorArg "constraintUnsatisfied" = (Address, MText)

instance CustomErrorHasDoc "constraintUnsatisfied" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Given constraint for given address wasn't satisfied."

safelistContract :: ContractCode Parameter Storage
safelistContract = compileIndigoContract safelistIndigo

safelistIndigo
  :: (HasStorage Storage, HasSideEffects)
  => Var Parameter -> IndigoProcedure '[Parameter, Storage, Ops]
safelistIndigo param = contractName "Safelist" $ do
  entryCase (Proxy @PlainEntryPointsKind) param
    ( #cAddAdmin /-> \addr -> do
        ensureSenderIsOwner
        addOrRemoveFromStorageSet addr #sAdmins Insert
    , #cRemoveAdmin /-> \addr -> do
        ensureSenderIsOwner
        addOrRemoveFromStorageSet addr #sAdmins Remove
    , #cIsAdmin /-> \v -> do
        doc $ DDescription
          "Check whether address is admin. Returns `True` if the address is admin."
        presentedInStorageSet v #sAdmins
    , #cAddToWhitelist /-> \addr -> do
        ensureSenderIsAdmin
        addOrRemoveFromStorageSet addr #sWhitelist Insert
    , #cRemoveFromWhitelist /-> \addr -> do
        ensureSenderIsAdmin
        addOrRemoveFromStorageSet addr #sWhitelist Remove
    , #cIsWhitelisted /-> \v -> do
        doc $ DDescription
          "Check whether address is whitelisted. Returns `True` if the address is \
          \whitelisted."
        presentedInStorageSet v #sWhitelist
    , #cAddToBlacklist /-> \addr -> do
        ensureSenderIsAdmin
        addOrRemoveFromStorageSet addr #sBlacklist Insert
    , #cRemoveFromBlacklist /-> \addr -> do
        ensureSenderIsAdmin
        addOrRemoveFromStorageSet addr #sBlacklist Remove
    , #cIsBlacklisted /-> \v -> do
        doc $ DDescription
          "Check whether address is blacklisted. Returns `True` if the address is \
          \blacklisted."
        presentedInStorageSet v #sBlacklist
    , #cEnsureSafelistConstraints /-> \constraints -> do
        doc $ DDescription $
          "Check list of constraints. For the first list check that all addresses are \
          \whitelisted, for the second list check that all addresses are non-blacklisted. \
          \If all constraints are satisfied, nothing happens, otherwise, it will fail \
          \on the first unsatisfied constraint."
        ensureSafelistConstraints constraints
    )

storage :: HasStorage Storage => Var Storage
storage = storageVar

data InsertOrRemove = Insert | Remove

ensureSenderIsOwner :: HasStorage Storage => IndigoProcedure s
ensureSenderIsOwner = do
  let
    ownerAddr = storage !. #sOwner
  if ownerAddr /=. Sender
  then failCustom_ #senderIsNotOwner
  else return ()

ensureSenderIsAdmin :: HasStorage Storage => IndigoProcedure s
ensureSenderIsAdmin = do
  let
    admins = storage !. #sAdmins
  if admins #?. Sender
  then return ()
  else failCustom_ #senderIsNotAdmin

ensureSafelistConstraints
  :: (HasStorage Storage, constraints :~> EnsureSafelistConstraintsParam)
  => constraints -> IndigoProcedure s
ensureSafelistConstraints constraints = do
  let
    isWhitelistedList = UnName #isWhitelisted $ fst constraints
    isNonBlacklistedList = UnName #isNonBlacklisted $ snd constraints
    whitelist = storage !. #sWhitelist
    blacklist = storage !. #sBlacklist
  forEach isWhitelistedList $ \it -> do
    if whitelist #?. it
    then return ()
    else failCustom #constraintUnsatisfied $ pair it $ [mt|nonWhitelisted|]
  forEach isNonBlacklistedList $ \it -> do
    if blacklist #?. it
    then failCustom #constraintUnsatisfied $ pair it $ [mt|blacklisted|]
    else return ()

presentedInStorageSet
  :: forall setName s.
     (HasFieldOfType Storage setName (Set Address), HasStorage Storage, HasSideEffects)
  => Var (View Address Bool) -> Label setName -> IndigoProcedure s
presentedInStorageSet v setName =
  flip view_ v $ \addr -> return $ (storage !. setName) #?. addr

addOrRemoveFromStorageSet
  :: forall setName s. (HasFieldOfType Storage setName (Set Address), HasStorage Storage)
  => Var Address -> Label setName -> InsertOrRemove -> IndigoProcedure s
addOrRemoveFromStorageSet addr setName insertOrRemove = do
  let
    requiredSet = storage !. setName
    newSet = case insertOrRemove of
      Insert -> requiredSet #~. addr
      Remove -> requiredSet #-. addr
  setField_ storage setName newSet
