{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Sync (sync) where

import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Integrations.Toolchain.Hie (syncHie)
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
    0
    "sync"
    [ ("enviroment", pure $ chalk Cyan $ format env),
      ("resolver", pure $ buildResolver env)
    ]
  sectionConfig
    0
    [ ("stack.yaml", syncStackYaml $> chalk Green "✓"),
      ("hie.yaml", syncHie $> chalk Green "✓")
    ]
  syncPackages
