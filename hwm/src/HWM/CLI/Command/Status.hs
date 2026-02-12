{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Status (showStatus) where

import HWM.Core.Formatting (Color (..), Format (..), chalk, genMaxLen, monadStatus, padDots, statusIcon, subPathSign)
import HWM.Core.Pkg (Pkg (..))
import qualified HWM.Domain.Config as C
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Matrix (getBuildEnvironment, getBuildEnvroments, printEnvironments)
import HWM.Domain.Workspace (memberPkgs, pkgGroupName)
import HWM.Integrations.Toolchain.Package (validatePackage)
import HWM.Runtime.UI (putLine, sectionTableM, sectionWorkspace)
import Relude

-- | Main status command - displays project info and package statuses
showStatus :: ConfigT ()
showStatus = do
  cfg <- asks config
  active <- getBuildEnvironment Nothing
  environments <- getBuildEnvroments
  sectionTableM
    0
    "project"
    [ ("name", pure $ chalk Magenta (C.name cfg)),
      ("version", pure $ chalk Green (format $ C.version cfg))
    ]
  printEnvironments active environments
  sectionWorkspace
    $ for_ (C.workspace cfg)
    $ \g -> do
      putLine ""
      putLine $ "• " <> chalk Bold (pkgGroupName g)
      pkgs <- memberPkgs g
      let maxLen = genMaxLen (map pkgMemberId pkgs)
      for_ pkgs $ \pkg -> do
        status <- monadStatus (validatePackage pkg)
        putLine $ subPathSign <> padDots maxLen (pkgMemberId pkg) <> statusIcon status
