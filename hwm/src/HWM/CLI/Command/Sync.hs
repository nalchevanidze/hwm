{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Sync (sync) where

import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Integrations.Toolchain.Cabal (syncCabalProject)
import HWM.Integrations.Toolchain.Hie (syncHie)
import HWM.Integrations.Toolchain.Nix (syncNixFile)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Integrations.Toolchain.Stack (syncStackYaml)
import HWM.Runtime.Cache (Registry (..), updateRegistry)
import HWM.Runtime.UI (sectionConfig, sectionTableM)
import Relude

sync :: Maybe Name -> ConfigT ()
sync tag = do
  env <- getBuildEnvironment tag
  updateRegistry $ \reg -> reg {currentEnv = buildName env}
  sectionTableM
    "sync"
    [ ("enviroment", pure $ chalk Cyan $ format env),
      ("resolver", pure $ buildResolver env)
    ]
  sectionConfig
    ( [("cabal.project", syncCabalProject)]
        <> [("stack.yaml", syncStackYaml) | buildStack env]
        <> [("flake.nix", syncNixFile) | buildNix env]
        <> [("hie.yaml", syncHie)]
    )
  syncPackages
