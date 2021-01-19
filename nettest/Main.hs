-- SPDX-FileCopyrightText: 2020 Globacap
--
-- SPDX-License-Identifier: MPL-2.0

-- Remove after switching to tasty nettest interface
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main (main) where

import Options.Applicative (execParser)

import Michelson.Runtime (prepareContract)
import Morley.Client (parserInfo)
import Morley.Nettest
import Morley.Nettest.Parser (mkNettestEnv, nettestConfigParser)
import Morley.Nettest.Pure (scenarioToIO)
import Util.Named

import Nettest.Holdings
import Nettest.WhitelistIntegration

main :: IO ()
main = do
  let clientParser = nettestConfigParser . pure $ Just "nettest.Globacap"
  parsedConfig <- execParser $
    parserInfo
      (#usage .! mempty)
      (#description .! "globacap testing scenario")
      (#header .! "globacap testing scenario")
      (#parser .! clientParser)
  env <- mkNettestEnv parsedConfig
  whitelistContract <- prepareContract $ Just "resources/whitelist.tz"

  putTextLn "Running pure holdings nettest scenario"
  scenarioToIO nettestScenario
  putTextLn "Running pure whitelist integration nettest scenario"
  scenarioToIO (whitelistScenario whitelistContract)

  putTextLn "Running holdings nettest scenario"
  runNettestClient env nettestScenario
  putTextLn "Running whitelist integration nettest scenario"
  runNettestClient env (whitelistScenario whitelistContract)
