{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HWM.CLI.Command.Add (runAdd, AddOptions (..)) where

import qualified Data.Set as S
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), chalk, genMaxLen, Format (..))
import HWM.Core.Pkg (Pkg (..), PkgName (..), pkgYamlPath)
import HWM.Domain.Bounds (Bounds (..))
import HWM.Domain.ConfigT (ConfigT, askWorkspaceGroups)
import HWM.Domain.Dependencies (Dependency (..), singleDeps)
import HWM.Domain.Workspace (memberPkgs, pkgGroupName, resolveTargets)
import HWM.Integrations.Toolchain.Cabal (syncCabal)
import HWM.Integrations.Toolchain.Package
import HWM.Runtime.Files (rewrite_, statusM)
import HWM.Runtime.UI (putLine)
import Relude

data AddOptions = AddOptions
  { packageName :: PkgName,
    workspaceId :: Name
  }
  deriving (Show)

runAdd :: AddOptions -> ConfigT ()
runAdd AddOptions {..} = do
  ws <- askWorkspaceGroups
  targets <- fmap (S.toList . S.fromList) (resolveTargets ws [workspaceId])
  let dependency = Dependency packageName (Bounds Nothing Nothing)
  putLine ""
  putLine $ "• " <> chalk Bold (format packageName) <> " will be added to the following packages:"
  let maxLen = genMaxLen (map pkgMemberId targets)
  for_ targets $ \pkg -> updatePackage maxLen (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg
