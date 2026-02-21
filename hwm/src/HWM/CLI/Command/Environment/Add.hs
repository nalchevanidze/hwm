{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd) where

import Control.Monad.Except (throwError)
import qualified Data.Map as M
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), padDots)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (Environments (..), existsEnviroment, newEnv, printEnvironments)
import HWM.Runtime.Cache (getSnapshotGHC)
import HWM.Runtime.Snapshots (SnapshotInfo (..), fetchLtsSuggestions, fetchStackageSnapshots)
import HWM.Runtime.UI (putLine, section)
import Options.Applicative (help, metavar, strArgument)
import Relude

data EnvAddOptions = EnvAddOptions
  { envName :: Name,
    envResolver :: Name
  }
  deriving (Show)

instance ParseCLI EnvAddOptions where
  parseCLI =
    EnvAddOptions
      <$> strArgument (metavar "NAME" <> help "Name of the environment to add")
      <*> strArgument (metavar "RESOLVER" <> help "Stackage resolver (e.g. lts-21.24)")

printSuggestions :: [Text] -> [Text] -> Text
printSuggestions [] alt
  | null alt = ""
  | otherwise = " Here are some available snapshots: " <> T.intercalate ", " alt <> "."
printSuggestions [s] _ = " Did you mean '" <> s <> "'?"
printSuggestions suggestions _ = " Did you mean one of: " <> T.intercalate ", " suggestions <> "?"

size :: Int
size = 16

runEnvAdd :: EnvAddOptions -> ConfigT ()
runEnvAdd EnvAddOptions {..} = do
  exists <- existsEnviroment envName
  if exists
    then do
      printEnvironments Nothing
      throwError $ fromString $ "Environment '" <> toString envName <> "' already exists."
    else section "new environment" $ do
      putLine $ padDots size "name" <> envName
      ltsMap <- fetchLtsSuggestions
      snapshots <- map snapshotName <$> fetchStackageSnapshots
      let suggestions = M.elems ltsMap <> take 12 (filter (not . isPrefixOf "nightly" . toString) snapshots)
      let prefixMatches = filter (isPrefixOf (toString envResolver) . toString) snapshots
      case M.lookup envResolver ltsMap <|> find (== envResolver) snapshots of
        Nothing -> throwError $ fromString $ "Resolver '" <> toString envResolver <> "' is not a valid Stackage snapshot." <> toString (printSuggestions prefixMatches suggestions)
        Just resolver -> do
          putLine $ padDots size "resolver" <> envName
          updateConfig
            ( \cfg@Config {..} -> do
                ghc <- getSnapshotGHC resolver
                putLine $ padDots size "ghc" <> format ghc
                pure cfg {matrix = matrix {environments = environments matrix <> [newEnv envName ghc resolver]}}
            )
            (pure ())
