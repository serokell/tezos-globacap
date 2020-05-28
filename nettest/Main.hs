-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0
module Main (main) where

import Options.Applicative (execParser)

import Michelson.Runtime (prepareContract)
import Morley.Nettest
import Util.Named

import Nettest.Holdings
import Nettest.WhitelistIntegration

main :: IO ()
main = do
  let clientParser = clientConfigParser . pure $ Just "nettest.Globacap"
  parsedConfig <- execParser $
    parserInfo
      (#usage .! mempty)
      (#description .! "globacap testing scenario")
      (#header .! "globacap testing scenario")
      (#parser .! clientParser)
  env <- mkMorleyClientEnv parsedConfig
  whitelistContract <- prepareContract $ Just "resources/whitelist.tz"

  putTextLn "Running pure holdings nettest scenario"
  runNettestViaIntegrational nettestScenario
  putTextLn "Running pure whitelist integration nettest scenario"
  runNettestViaIntegrational (whitelistScenario whitelistContract)

  putTextLn "Running holdings nettest scenario"
  runNettestClient env nettestScenario
  putTextLn "Running whitelist integration nettest scenario"
  runNettestClient env (whitelistScenario whitelistContract)
