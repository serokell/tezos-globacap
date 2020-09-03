-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Indigo.Contracts.Holdings.Types
  ( Parameter (..)
  , Storage
  , StorageFields (..)
  , TokenMeta (..)
  , dummyMeta
  , mkStorage
  , storageNotes
  )
where

import Indigo

import qualified Data.Map as Map
import Fmt (Buildable(..), (+|), (|+))

import qualified Lorentz.Contracts.ManagedLedger.Types as ML hiding (Storage)
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as ML
import Michelson.Typed (Notes(..), starNotes)
import Michelson.Untyped (ann, noAnn)
import Util.Named

data StorageFields = StorageFields
  { sfTokenMeta :: TokenMeta
  , sfMbSafelistAddress :: Maybe Address
  , sfOwner :: Address
  , sfAdmin :: Address
  , sfMbNewAdmin :: Maybe Address
  , sfPaused :: Bool
  , sfTransferable :: Bool
  , sfTotalSupply :: Natural
  }
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

data TokenMeta = TokenMeta
  { tmName :: MText
  , tmSymbol :: MText
  , tmId :: MText
  }
  deriving stock (Eq, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance HasField TokenMeta "tokenName" MText where
  fieldLens = fieldLensADT #tmName

instance HasField TokenMeta "tokenSymbol" MText where
  fieldLens = fieldLensADT #tmSymbol

instance HasField StorageFields "tokenName" MText where
  fieldLens = fieldLensDeeper #sfTokenMeta

instance HasField StorageFields "tokenSymbol" MText where
  fieldLens = fieldLensDeeper #sfTokenMeta

instance HasField StorageFields "tokenMeta" TokenMeta where
  fieldLens = fieldLensADT #sfTokenMeta

instance HasField StorageFields "mbSafelistAddress" (Maybe Address) where
  fieldLens = fieldLensADT #sfMbSafelistAddress

instance HasField StorageFields "owner" Address where
  fieldLens = fieldLensADT #sfOwner

instance HasField StorageFields "admin" Address where
  fieldLens = fieldLensADT #sfAdmin

instance HasField StorageFields "mbNewAdmin" (Maybe Address) where
  fieldLens = fieldLensADT #sfMbNewAdmin

instance HasField StorageFields "paused" Bool where
  fieldLens = fieldLensADT #sfPaused

instance HasField StorageFields "transferable" Bool where
  fieldLens = fieldLensADT #sfTransferable

instance HasField StorageFields "totalSupply" Natural where
  fieldLens = fieldLensADT #sfTotalSupply

instance HasField Storage "tokenName" MText where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "tokenSymbol" MText where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "tokenMeta" TokenMeta where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "mbSafelistAddress" (Maybe Address) where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "owner" Address where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "admin" Address where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "mbNewAdmin" (Maybe Address) where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "paused" Bool where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "transferable" Bool where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "totalSupply" Natural where
  fieldLens = fieldLensDeeper #sFields

instance HasField Storage "ledger" (BigMap Address ML.LedgerValue) where
  fieldLens = fieldLensADT #sLedger

instance HasField Storage "approvals" (BigMap ML.GetAllowanceParams Natural) where
  fieldLens = fieldLensADT #sApprovals

instance Buildable TokenMeta where
  build TokenMeta{..} =
    "Token meta: name = '" +| tmName |+ "', symbol = '" +| tmSymbol |+
    "', id = '" +| tmId |+ "'"

instance TypeHasDoc TokenMeta where
  typeDocMdDescription = "Meta information about token."

dummyMeta :: TokenMeta
dummyMeta = TokenMeta [mt|KekToken|] [mt|Kek|] [mt|Ququareq|]

data Storage = Storage
  { sLedger    :: BigMap Address ML.LedgerValue
  , sApprovals :: BigMap ML.GetAllowanceParams Natural
  , sFields    :: StorageFields
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Storage where
  typeDocName _ = "Holdings storage"
  typeDocMdDescription =
    "Root datatype for Holdings contract storage type. It contains two `big_map`s: \n\
    \* `ledger` - stores addresses balances.\n\
    \* `approvals` - stores approvals amounts.\n\n\
    \Apart from that it has `fields` which store various additional information \
    \about contract state. See `Holdings storage fields` type."

instance TypeHasDoc StorageFields where
  typeDocName _ = "Holdings storage fields"
  typeDocMdDescription =
    "Additional contract fields that define the current contract state. \
    \It stores meta-information about token (see `TokenMeta` type), \
    \information about the `owner` and the current `admin` of the contract. \
    \Additionally it stores an optional address for currently used Safelist \
    \contract, the `totalSupply` amount, and the `paused` and `transferable` flags."

storageNotes :: Notes (ToT Storage)
storageNotes =  ML.storageSkeletonNotes @StorageFields $
  NTPair noAnn noAnn noAnn
   (NTPair noAnn noAnn noAnn
     (NTPair noAnn (ann "tokenMeta") (ann "mbSafelistAddress") tokenMetaNotes starNotes)
     (NTPair noAnn (ann "owner") (ann "admin") starNotes starNotes)
   )
   (NTPair noAnn noAnn noAnn
     (NTPair noAnn (ann "mbNewAdmin") (ann "paused") starNotes starNotes)
     (NTPair noAnn (ann "transferable") (ann "totalSupply") starNotes starNotes)
   )

tokenMetaNotes :: Notes (ToT TokenMeta)
tokenMetaNotes = NTPair noAnn (ann "name") noAnn
  starNotes
  (NTPair noAnn (ann "symbol") (ann "id") starNotes starNotes)

mkStorage
  :: Address -> Address -> Maybe Address -> [(Address, Natural)] -> Natural
  -> TokenMeta -> Storage
mkStorage owner admin mbSafelist balances totalSupply meta = Storage
  { sLedger = BigMap $ (#balance .!) <$> (Map.fromList balances)
  , sApprovals = mempty
  , sFields = fields
  }
  where
    fields :: StorageFields
    fields = StorageFields
      { sfTokenMeta = meta
      , sfMbSafelistAddress = mbSafelist
      , sfOwner = owner
      , sfAdmin = admin
      , sfMbNewAdmin = Nothing
      , sfPaused = False
      , sfTransferable = True
      , sfTotalSupply = totalSupply
      }

data Parameter
  = SetName ("newName" :! MText)
  | SetSymbol ("newSymbol" :! MText)
  | SetSafelistAddress ("newMbSafelistAddress" :! (Maybe Address))
  | TransferAdminRights ("newAdmin" :! Address)
  | AcceptAdminRights ()
  | IsAdmin (View Address Bool)
  | Transfer ML.TransferParams
  | Seize ML.TransferParams
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

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain
