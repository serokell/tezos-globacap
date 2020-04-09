-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
module Main (main) where

import Michelson.Runtime (prepareContract)
import Morley.Nettest

import Nettest.Holdings
import Nettest.WhitelistIntegration

main :: IO ()
main = do
  config <- getClientConfig (Just "Globacap")
  putTextLn "Running holdings nettest scenario"
  runNettestClient config nettestScenario
  whitelistContract <- prepareContract $ Just "resources/whitelist.tz"
  putTextLn "Running whitelist integration nettest scenario"
  runNettestClient config (whitelistScenario whitelistContract)
