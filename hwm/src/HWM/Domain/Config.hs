{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Config
  ( Config (..),
    getRule,
    defaultScripts,
  )
where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), genericParseJSON, genericToJSON)
import qualified Data.Map as Map
import HWM.Core.Common (Check (..), Name)
import HWM.Core.Has (Has)
import HWM.Core.Pkg
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Bounds (Bounds, versionBounds)
import HWM.Domain.Dependencies (Dependencies, getBounds)
import HWM.Domain.Environments (Environments (..))
import HWM.Domain.Release (Release)
import HWM.Domain.Workspace (PkgRegistry, Workspace)
import HWM.Runtime.Cache (Cache)
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
import Relude

data Config = Config
  { cfgName :: Name,
    cfgGithub :: Maybe Text,
    cfgVersion :: Version,
    cfgBounds :: Maybe Bounds,
    cfgWorkspace :: Workspace,
    cfgEnvironments :: Environments,
    cfgRegistry :: Dependencies,
    cfgScripts :: Map Name Text,
    cfgRelease :: Maybe Release
  }
  deriving
    ( Generic,
      Show
    )

getRule :: (MonadError Issue m) => PkgName -> PkgRegistry -> Config -> m Bounds
getRule depName ps Config {..}
  | Map.member depName ps = pure (fromMaybe (versionBounds cfgVersion) cfgBounds)
  | otherwise = getBounds depName cfgRegistry

prefix :: String
prefix = "cfg"

instance FromJSON Config where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced prefix)

instance ToJSON Config where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced prefix)

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env Cache,
    Has env Workspace,
    Has env Environments,
    MonadIO m
  ) =>
  Check m Config
  where
  check Config {..} = check cfgEnvironments

defaultScripts :: Map Name Text
defaultScripts =
  Map.fromList
    [ ("build", "stack build --fast"),
      ("test", "stack test {TARGET} --fast"),
      ("install", "stack install"),
      ("lint", "curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s ."),
      ("clean", "find . -name \"*.cabal\" -exec rm -rf {} \\; && stack clean")
    ]
