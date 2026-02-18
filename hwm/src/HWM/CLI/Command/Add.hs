{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HWM.CLI.Command.Add (runAdd, AddOptions (..)) where

import qualified Data.Set as S
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..), chalk, genMaxLen, padDots)
import HWM.Core.Pkg (Pkg (..), PkgName (..), pkgId, pkgYamlPath)
import HWM.Domain.Bounds (Bounds (..), deriveBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (config), askWorkspaceGroups, updateConfig)
import HWM.Domain.Dependencies (Dependency (..), lookupBounds, singleDeps)
import HWM.Domain.Matrix (getTestedRange)
import HWM.Domain.Workspace (memberPkgs, pkgGroupName, resolveTargets)
import HWM.Integrations.Toolchain.Cabal (syncCabal)
import HWM.Integrations.Toolchain.Package
import HWM.Runtime.Files (rewrite_, statusM)
import HWM.Runtime.UI (putLine, section, sectionConfig, sectionTableM)
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

  sectionTableM
    0
    "add dependency"
    [ ("package", pure $ chalk Magenta (format packageName)),
      ("target", pure $ chalk Cyan (format (T.intercalate ", " (map pkgId targets))))
    ]

  registered <- asks (lookupBounds packageName . registry . config)
  case registered of
    Nothing -> do
      section "discovery" $ do
        putLine $ padDots 16 "registry" <> "missing (initiating lookup)"

      -- "lts-18.10" ...... 1.5.6.0 (min)
      -- "nightly" ........ 2.2.3.0 (max)

      bounds <- maybe (getTestedRange >>= deriveBounds packageName) pure registered
      let dependency = Dependency packageName bounds

      ((\cf -> pure cf {registry = registry cf <> singleDeps dependency}) `updateConfig`) $ do
        sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
        addDepToPackage targets dependency
    Just bounds -> do
      section "discovery" $ do
        putLine $ padDots 16 "registry" <> format bounds <> " (already registered)"
      addDepToPackage targets (Dependency packageName bounds)
  where
    addDepToPackage targets dependency = do
      putLine ""
      putLine $ "• " <> chalk Bold (format packageName) <> " will be added to the following packages:"
      let maxLen = genMaxLen (map pkgMemberId targets)
      for_ targets $ \pkg -> updatePackage maxLen (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg
