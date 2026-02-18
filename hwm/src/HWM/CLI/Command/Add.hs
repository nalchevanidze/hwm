{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HWM.CLI.Command.Add (runAdd, AddOptions (..)) where

import HWM.Core.Common (Name)
import HWM.Domain.ConfigT (ConfigT, askWorkspaceGroups)
import HWM.Runtime.UI (putLine)
import Relude
import qualified Data.Set as S
import HWM.Domain.Workspace (resolveTargets, pkgGroupName, memberPkgs)
import HWM.Core.Formatting (chalk, Color (..), genMaxLen)
import HWM.Core.Pkg (Pkg(..), pkgYamlPath)
import HWM.Runtime.Files (rewrite_, statusM)
import HWM.Integrations.Toolchain.Cabal (syncCabal)
import HWM.Integrations.Toolchain.Package

data AddOptions = AddOptions
  { packageName :: Name,
    workspaceId :: Name
  }
  deriving (Show)

runAdd :: AddOptions -> ConfigT ()
runAdd AddOptions {..} = do
  ws <- askWorkspaceGroups
  targets <- fmap (S.toList . S.fromList) (resolveTargets ws [workspaceId])
  putLine ""
  putLine $ "• " <> chalk Bold packageName
  let maxLen = genMaxLen (map pkgMemberId targets)
  for_ targets $ \pkg -> updatePackage maxLen (packageUpdateDependencies pkg) pkg

