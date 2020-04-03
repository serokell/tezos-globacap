-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Main (main) where

import Morley.Nettest

import Nettest.Holdings

main :: IO ()
main = do
  config <- getClientConfig (Just "Globacap")
  runNettestClient config nettestScenario
