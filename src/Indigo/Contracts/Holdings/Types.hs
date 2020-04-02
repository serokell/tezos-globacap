-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Indigo.Contracts.Holdings.Types
  ( Parameter (..)
  , Storage
  , StorageFields (..)
  , TokenMeta (..)
  , dummyMeta
  , mkStorage
  )
where

import Indigo

import Fmt (Buildable(..), (+|), (|+))

import qualified Lorentz.Contracts.ManagedLedger.Types as ML
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as ML
import Lorentz.TypeAnns (HasTypeAnn)

data StorageFields = StorageFields
  { tokenMeta :: TokenMeta
  , mbSafelistAddress :: Maybe Address
  , owner :: Address
  , admin :: Address
  , mbNewAdmin :: Maybe Address
  , paused :: Bool
  , transferable :: Bool
  , totalSupply :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

data TokenMeta = TokenMeta
  { tmName :: MText
  , tmSymbol :: MText
  , tmId :: MText
  }
  deriving stock (Eq, Generic)
  deriving anyclass (IsoValue, HasTypeAnn)

instance StoreHasField TokenMeta "tokenName" MText where
  storeFieldOps = storeFieldOpsReferTo #tmName storeFieldOpsADT

instance StoreHasField TokenMeta "tokenSymbol" MText where
  storeFieldOps = storeFieldOpsReferTo #tmSymbol storeFieldOpsADT

instance StoreHasField StorageFields "tokenMeta" TokenMeta where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "tokenName" MText where
  storeFieldOps = storeFieldOpsDeeper #tokenMeta

instance StoreHasField StorageFields "tokenSymbol" MText where
  storeFieldOps = storeFieldOpsDeeper #tokenMeta

instance StoreHasField StorageFields "mbSafelistAddress" (Maybe Address) where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "owner" Address where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "admin" Address where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "mbNewAdmin" (Maybe Address) where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "paused" Bool where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "transferable" Bool where
  storeFieldOps = storeFieldOpsADT

instance StoreHasField StorageFields "totalSupply" Natural where
  storeFieldOps = storeFieldOpsADT

instance Buildable TokenMeta where
  build TokenMeta{..} =
    "Token meta: name = '" +| tmName |+ "', symbol = '" +| tmSymbol |+
    "', id = '" +| tmId |+ "'"

instance TypeHasDoc TokenMeta where
  typeDocMdDescription = "Meta information about token."

dummyMeta :: TokenMeta
dummyMeta = TokenMeta [mt|KekToken|] [mt|Kek|] [mt|Ququareq|]

type Storage = ML.StorageSkeleton StorageFields

mkStorage :: Address -> Maybe Address -> TokenMeta -> Storage
mkStorage owner mbSafelist meta =
  ML.mkStorageSkeleton mempty fields
  where
    fields :: StorageFields
    fields = StorageFields
      { tokenMeta = meta
      , mbSafelistAddress = mbSafelist
      , owner = owner
      , admin = owner
      , mbNewAdmin = Nothing
      , paused = False
      , transferable = True
      , totalSupply = 0
      }

data Parameter
  = SetName ("newName" :! MText)
  | SetSymbol ("newSymbol" :! MText)
  | SetSafelistAddress ("newMbSafelistAddress" :! (Maybe Address))
  | TransferAdminRights ("newAdmin" :! Address)
  | AcceptAdminRights ()
  | IsAdmin (View Address Bool)
  | Transfer ML.TransferParams
  | Approve ML.ApproveParams
  | GetAllowance (View ML.GetAllowanceParams Natural)
  | GetBalance (View ML.GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  | Mint ML.MintParams
  | Burn ML.BurnParams
  | BurnAll ()
  | SetPause ("value" :! Bool)
  | SetTransferable ("value" :! Bool)
  | GetTokenMeta (View () TokenMeta)
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain
