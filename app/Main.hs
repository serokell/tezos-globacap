-- SPDX-FileCopyrightText: 2020 TBD
--
-- SPDX-License-Identifier: LicenseRef-Proprietary
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import qualified Data.Map as Map
import Data.Version (showVersion)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_globacap (version)

import qualified Lorentz as L
import Lorentz (mt)
import Lorentz.ContractRegistry
import Michelson.Typed (starNotes)
import Morley.CLI (addressOption, mTextOption)
import Tezos.Address (Address)
import Util.CLI (mkCLOptionParser)
import Util.Named

import Indigo.Contracts.Holdings (holdingsContract)
import qualified Indigo.Contracts.Holdings as H

programInfo :: L.DGitRevision -> Opt.ParserInfo CmdLnArgs
programInfo gitRev = Opt.info (Opt.helper <*> versionOption <*> argParser contracts gitRev) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Tezos Globacap contracts registry"
  , Opt.header "Contracts for Tezos Globacap project"
  , Opt.footerDoc usageDoc
  ]
  where
    versionOption = Opt.infoOption ("globacap-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for a specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  globacap print --help", linebreak
   ]

contracts :: ContractRegistry
contracts = ContractRegistry $ Map.fromList
  [ "Holdings" ?:: ContractInfo
    { ciContract = holdingsContract
    , ciIsDocumented = True
    , ciStorageParser = Just holdingsStorageParser
    , ciCompilationOptions = L.defaultCompilationOptions
    , ciStorageNotes = starNotes
    }
  ]

holdingsStorageParser :: Opt.Parser H.Storage
holdingsStorageParser = do
  owner <-
    addressOption Nothing (#name .! "owner")
    (#help .! "Contract owner address.")
  admin <-
    addressOption Nothing (#name .! "admin")
    (#help .! "Initial administrator address.")
  mbSafelist <- mbAddressOption "safelist-address" "Initial safelist address"
  tmName <- mTextOption (Just [mt|Token|]) (#name .! "token-name")
    (#help .! "Initial token name.")
  tmSymbol <- mTextOption (Just [mt|T|]) (#name .! "token-symbol")
    (#help .! "Initial token symbol.")
  tmId <- mTextOption (Just [mt|Token-id|]) (#name .! "token-id")
    (#help .! "Initial token id.")
  pure $ H.mkStorage owner admin mbSafelist H.TokenMeta{..}
  where
    mbAddressOption :: String -> String -> Opt.Parser (Maybe Address)
    mbAddressOption name hInfo = optional $
      (mkCLOptionParser Nothing (#name .! name) (#help .! hInfo))

main :: IO ()
main = withUtf8 $ do
  let gitRev = $(L.mkDGitRevision) $ L.GitRepoSettings $ \commit ->
        "https://github.com/serokell/tezos-globacap/blob/" <> commit
  cmdLnArgs <- Opt.execParser $ programInfo gitRev
  runContractRegistry contracts cmdLnArgs `catchAny` (die . displayException)
