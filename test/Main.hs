-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Main
  ( main
  ) where

import Test.Tasty (defaultMain)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
