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
import HWM.Domain.Bounds (Bounds)
import HWM.Domain.Dependencies (Dependencies, getBounds)
import HWM.Domain.Matrix (Matrix (..))
import HWM.Domain.Workspace (PkgRegistry, WorkspaceGroup)
import HWM.Runtime.Cache (Cache)
import HWM.Runtime.Files (aesonYAMLOptions)
import Relude

data Config = Config
  { name :: Name,
    version :: Version,
    bounds :: Bounds,
    workspace :: [WorkspaceGroup],
    matrix :: Matrix,
    registry :: Dependencies,
    scripts :: Map Name Text
  }
  deriving
    ( Generic,
      Show
    )

getRule :: (MonadError Issue m) => PkgName -> PkgRegistry -> Config -> m Bounds
getRule depName ps Config {..}
  | Map.member depName ps = pure bounds
  | otherwise = getBounds depName registry

instance FromJSON Config where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Config where
  toJSON = genericToJSON aesonYAMLOptions

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env Cache,
    Has env [WorkspaceGroup],
    Has env Matrix,
    MonadIO m
  ) =>
  Check m Config
  where
  check Config {..} = check matrix

defaultScripts :: Map Name Text
defaultScripts =
  Map.fromList
    [ ("build", "stack build --fast"),
      ("test", "stack test {TARGET} --fast"),
      ("install", "stack install"),
      ("lint", "curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s ."),
      ("clean", "find . -name \"*.cabal\" -exec rm -rf {} \\; && stack clean")
    ]
