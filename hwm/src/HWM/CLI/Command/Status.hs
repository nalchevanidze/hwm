{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Status (showStatus) where

import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Environments (printEnvironments)
import HWM.Domain.Workspace (forWorkspace)
import HWM.Integrations.Toolchain.Package (validatePackage)
import HWM.Runtime.UI (sectionTableM)
import Relude

-- | Main status command - displays project info and package statuses
showStatus :: ConfigT ()
showStatus = do
  cfg <- asks config
  sectionTableM
    "project"
    [ ("name", pure $ chalk Magenta (cfgName cfg)),
      ("version", pure $ chalk Green (format $ cfgVersion cfg))
    ]
  printEnvironments Nothing
  forWorkspace validatePackage
