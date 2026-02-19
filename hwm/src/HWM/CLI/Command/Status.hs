{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Status (showStatus) where

import HWM.Core.Formatting (Color (..), Format (..), chalk)
import qualified HWM.Domain.Config as C
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Matrix (printEnvironments)
import HWM.Domain.Workspace (printWorkspace)
import HWM.Integrations.Toolchain.Package (validatePackage)
import HWM.Runtime.UI (sectionTableM)
import Relude

-- | Main status command - displays project info and package statuses
showStatus :: ConfigT ()
showStatus = do
  cfg <- asks config
  sectionTableM
    0
    "project"
    [ ("name", pure $ chalk Magenta (C.name cfg)),
      ("version", pure $ chalk Green (format $ C.version cfg))
    ]
  printEnvironments Nothing
  printWorkspace validatePackage
