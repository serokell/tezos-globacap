-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Test.Indigo.Contracts.Common
  ( expectStorageUpdate
  , originateHoldings
  , originateHoldingsWithAlSettings
  , originateSafelist
  ) where

import Lorentz (Address, NiceStorage, TAddress(..), ToAddress)
import Lorentz.Contracts.Test (AlSettings(..))
import Lorentz.Test
import Tezos.Core (toMutez)

import qualified Indigo.Contracts.Holdings as H
import qualified Indigo.Contracts.Safelist as SL

originateSafelist
  :: [(Address, Address)] -> [Address] -> IntegrationalScenarioM (TAddress SL.Parameter)
originateSafelist transfers receivers =
  lOriginate SL.safelistContract "safelist" (SL.mkStorage transfers receivers) (toMutez 0)

originateHoldingsWithAlSettings
  :: Address -> AlSettings -> IntegrationalScenarioM (TAddress H.Parameter)
originateHoldingsWithAlSettings admin (AlInitAddresses initAddresses) =
  lOriginate H.holdingsContract "holdings"
  (H.mkStorage admin admin Nothing initAddresses (sum $ map snd initAddresses) H.dummyMeta)
  (toMutez 0)

originateHoldings
  :: Address -> Maybe Address
  -> IntegrationalScenarioM (TAddress H.Parameter)
originateHoldings owner mbSafelist =
  lOriginate H.holdingsContract "holdings"
  (H.mkStorage owner owner mbSafelist mempty 0 H.dummyMeta) (toMutez 0)

expectStorageUpdate
  :: (ToAddress addr, NiceStorage st)
  => addr -> (st -> Bool) -> Text -> SuccessValidator
expectStorageUpdate contractAddr validateStorage msg =
  lExpectStorageUpdate contractAddr
    (\st -> bool
      (Left $ CustomValidationError msg) (Right ()) $ validateStorage st
    )
