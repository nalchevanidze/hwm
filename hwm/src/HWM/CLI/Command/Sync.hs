{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Sync (sync) where

import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..), Status (..), chalk)
import HWM.Core.Sync (SyncMode (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..), StackBuildConfig (..), TargetModes (..), getBuildEnvironment)
import HWM.Integrations.Toolchain.Cabal (syncCabalProject)
import HWM.Integrations.Toolchain.Hie (syncHie)
import HWM.Integrations.Toolchain.Nix (syncNixFile)
import HWM.Integrations.Toolchain.Package (syncPackagesByMode)
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
    [ ("environment", pure $ chalk Cyan $ format env),
      ("resolver", pure $ stackResolver (buildStack env))
    ]
  sectionConfig (syncTargets env)
  syncPackagesByMode (targetPackages $ buildTargets env)

syncTargets :: BuildEnvironment -> [(Text, ConfigT Status)]
syncTargets env =
  [ (label, action mode)
    | (mode, label, action) <- targets,
      mode /= SyncModeIgnore
  ]
  where
    modes = buildTargets env
    targets =
      [ (targetCabal modes, "cabal.project", syncCabalProject),
        (targetStack modes, "stack.yaml", syncStackYaml),
        (targetNix modes, "flake.nix", syncNixFile),
        (targetHie modes, "hie.yaml", syncHie)
      ]

