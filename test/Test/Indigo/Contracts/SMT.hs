-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Test.Indigo.Contracts.SMT
  ( test_holdingsApprovableLedgerSMT
  ) where

import Test.Tasty (TestTree)

import Lorentz.Contracts.Test (approvableLedgerSMT)

import Test.Indigo.Contracts.Common

-- | Without safelist checks our contract should satisfy state machine
-- tests for compliance to FA1.2/Approvable Ledger interface.
test_holdingsApprovableLedgerSMT :: [TestTree]
test_holdingsApprovableLedgerSMT =
  approvableLedgerSMT originateHoldingsWithAlSettings
