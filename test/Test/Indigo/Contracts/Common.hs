-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Test.Indigo.Contracts.Common
  ( expectStorageUpdate
  , originateSafelist
  ) where

import Lorentz (Address, NiceStorage, TAddress(..), ToAddress)
import Lorentz.Test
import Tezos.Core (toMutez)

import qualified Indigo.Contracts.Safelist as SL

originateSafelist
  :: [(Address, Address)] -> [Address] -> IntegrationalScenarioM (TAddress SL.Parameter)
originateSafelist transfers receivers =
  lOriginate SL.safelistContract "safelist" (SL.mkStorage transfers receivers) (toMutez 0)


expectStorageUpdate
  :: (ToAddress addr, NiceStorage st)
  => addr -> (st -> Bool) -> Text -> SuccessValidator
expectStorageUpdate contractAddr validateStorage msg =
  lExpectStorageUpdate contractAddr
    (\st -> bool
      (Left $ CustomValidationError msg) (Right ()) $ validateStorage st
    )
