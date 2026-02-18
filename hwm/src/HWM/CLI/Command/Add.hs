{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HWM.CLI.Command.Add (runAdd, AddOptions (..)) where

import qualified Data.Set as S
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..), chalk, genMaxLen)
import HWM.Core.Pkg (Pkg (..), PkgName (..), pkgYamlPath)
import HWM.Domain.Bounds (Bounds (..), testedBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (config), askWorkspaceGroups)
import HWM.Domain.Dependencies (Dependency (..), lookupBounds, singleDeps)
import HWM.Domain.Matrix (getTestedRange)
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
  originalRegistry <- asks (registry . config)
  bounds <- maybe (testedBounds packageName <$> getTestedRange) pure (lookupBounds packageName originalRegistry)

  let dependency = Dependency packageName bounds
  putLine ""
  putLine $ "• " <> chalk Bold (format packageName) <> " will be added to the following packages:"
  let maxLen = genMaxLen (map pkgMemberId targets)
  for_ targets $ \pkg -> updatePackage maxLen (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg
