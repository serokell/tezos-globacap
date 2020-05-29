-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-orphans #-}
module Indigo.Contracts.Common.Error
  (
  ) where

import Indigo

type instance ErrorArg "senderIsNotOwner" = ()

instance CustomErrorHasDoc "senderIsNotOwner" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Sender is not the contract owner."
