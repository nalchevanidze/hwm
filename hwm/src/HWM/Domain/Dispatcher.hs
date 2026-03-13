{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Dispatcher
  ( dispatchForEach,
    dispatch,
    DispatcheCommand (..),
  )
where

import Data.List (intersect, (\\))
import HWM.Core.Formatting (Color (..), Format (..), andMore, chalk)
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.Build (Builder (..), BuilderCommand (..), TargetScope (..), comandLogId, isCustom, postBuildAction, toExec)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..))
import HWM.Domain.Workspace (printPkgWSRef)
import HWM.Integrations.Toolchain.Stack (genStackMatrixConfig, getStackMatrixEnvVars)
import HWM.Runtime.Process (execInBackground)
import HWM.Runtime.UI (minRowSize, sectionEnvironments, section_, uiRow)
import Relude

data DispatcheCommand = DispatcheCommand
  { builderCommand :: BuilderCommand,
    scope :: TargetScope,
    commandFlags :: [Text]
  }

dispatch :: DispatcheCommand -> BuildEnvironment -> ConfigT ()
dispatch (DispatcheCommand cmd tscope flags) env@BuildEnvironment {..} = do
  scope <- excludePackages buildPkgs tscope
  genStackMatrixConfig env
  envs <- getStackMatrixEnvVars buildName
  exec <- toExec buildBuilder cmd scope flags envs
  execInBackground nixEnabled exec (comandLogId cmd) buildName minRowSize
  postBuildAction buildBuilder cmd scope
  where
    nixEnabled = buildNix && (buildBuilder /= NixBuilder || isCustom cmd)

excludePackages :: [Pkg] -> TargetScope -> ConfigT TargetScope
excludePackages _ ScopeGlobal = pure ScopeGlobal
excludePackages supported (ScopePkgs pkgs) = do
  case pkgs \\ supported of
    [] -> pure ()
    (pkg : xs) -> uiRow minRowSize "excluded" (chalk Dim (andMore (printPkgWSRef pkg) (map printPkgWSRef xs)))
  pure $ ScopePkgs (pkgs `intersect` supported)

dispatchForEach :: (Foldable t) => DispatcheCommand -> t BuildEnvironment -> ConfigT ()
dispatchForEach dcmd envs = sectionEnvironments Nothing
  $ for_ envs
  $ \env -> section_ (format env) $ dispatch dcmd env
