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
import HWM.Core.Formatting (Color (..), Format (..), andMore, chalk, indentBlockNum, padDots)
import HWM.Core.Options (isCI)
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.Build (BuildFlag (..), Builder (..), BuilderCommand (..), comandLabel, toExec)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..))
import HWM.Domain.Schema (TargetScope (..))
import HWM.Domain.Workspace (printPkgWSRef)
import HWM.Integrations.Toolchain.Cabal (setupCabalMatrixEnvironment)
import HWM.Integrations.Toolchain.Stack (setupStackMatrixEnvironment)
import HWM.Runtime.Process (EnvVars, ExecOptions (..), execInBackground)
import HWM.Runtime.UI (MonadUI (..), minRowSize, sectionEnvironments, section_, uiRow)
import Relude

data DispatcheCommand = DispatcheCommand
  { builderCommand :: BuilderCommand,
    scope :: TargetScope,
    commandFlags :: [BuildFlag]
  }

setupEnvironment :: BuildEnvironment -> ConfigT EnvVars
setupEnvironment env =
  case buildBuilder env of
    StackBuilder -> setupStackMatrixEnvironment env
    CabalBuilder {} -> setupCabalMatrixEnvironment env
    NixBuilder -> pure mempty

dispatch :: DispatcheCommand -> BuildEnvironment -> ConfigT ()
dispatch (DispatcheCommand cmd tscope flags) env@BuildEnvironment {..} = do
  envs <- setupEnvironment env
  scope <- excludePackages buildPkgs tscope
  exec <- toExec buildName buildBuilder cmd scope flags envs
  ci <- isCI
  ind <- uiIndentLevel
  execInBackground
    exec
    ExecOptions
      { envName = buildName,
        formatFX = \path icon -> indentBlockNum ind (padDots minRowSize (comandLabel cmd) <> icon <> chalk Dim (" logs: " <> path)),
        fxEnabled = not ci
      }

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
