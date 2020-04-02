-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
{-# OPTIONS_GHC -Wno-orphans #-}
module Indigo.Contracts.Holdings.Error
  (
  ) where

import Indigo

type instance ErrorArg "notInTransferAdminRightsMode" = ()

instance CustomErrorHasDoc "notInTransferAdminRightsMode" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Cannot accept admin rights before transfer process has been initiated \
    \by calling transferAdminRights entrypoint."

type instance ErrorArg "senderIsNotNewAdmin" = ()

instance CustomErrorHasDoc "senderIsNotNewAdmin" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Cannot accept admin rights because the sender address is different from \
    \the address passed to the transferadminRights entrypoint previously."

type instance ErrorArg "invalidSafelistAddr" = MText

instance CustomErrorHasDoc "invalidSafelistAddr" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "New safelist address doesn't have required entrypoint"

type instance ErrorArg "nonTransferable" = ()

instance CustomErrorHasDoc "nonTransferable" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Transferable flag is false. Transfers are prohibited."
