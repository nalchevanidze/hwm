{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd) where

import Control.Monad.Except (throwError)
import qualified Data.Map as M
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Matrix (BuildEnv (..), Matrix (..))
import HWM.Runtime.Cache (getSnapshotGHC)
import HWM.Runtime.Snapshots (SnapshotInfo (..), fetchLtsSuggestions, fetchStackageSnapshots)
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

runEnvAdd :: EnvAddOptions -> ConfigT ()
runEnvAdd EnvAddOptions {..} = do
  shortLtsMap <- fetchLtsSuggestions
  snapshots <- map snapshotName <$> fetchStackageSnapshots
  let suggestions = M.elems shortLtsMap <> take 12 (filter (not . isPrefixOf "nightly" . toString) snapshots)

  case M.lookup envResolver shortLtsMap <|> find (== envResolver) snapshots of
    Nothing -> do
      let prefixMatches = filter (isPrefixOf (toString envResolver) . toString) snapshots
      let suggestionMsg = case prefixMatches of
            [] ->
              if null suggestions
                then ""
                else " Here are some available snapshots: " <> T.intercalate ", " suggestions <> "."
            [s] -> " Did you mean '" <> s <> "'?"
            suggestions -> " Did you mean one of: " <> T.intercalate ", " suggestions <> "?"
      throwError $ fromString $ "Resolver '" <> toString envResolver <> "' is not a valid Stackage snapshot." <> toString suggestionMsg
    Just snapshot -> do
      ghc <- getSnapshotGHC snapshot
      let newEnv =
            BuildEnv
              { name = envName,
                ghc = ghc,
                resolver = snapshot,
                extraDeps = Nothing,
                exclude = Nothing,
                allowNewer = Nothing
              }
      updateConfig
        ( \cfg@Config {..} ->
            let envs' = environments matrix ++ [newEnv]
                matrix' = matrix {environments = envs'}
             in pure cfg {matrix = matrix'}
        )
        (pure ())
