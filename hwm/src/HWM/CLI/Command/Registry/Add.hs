{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Add (runRegistryAdd, RegistryAddOptions (..)) where

-- removed accidental self-import

import qualified Data.Set as S
import qualified Data.Text as T
import HWM.Core.Formatting (Color (..), Format (..), chalk, genMaxLen, padDots)
import HWM.Core.Parsing (ParseCLI (..), parse)
import HWM.Core.Pkg (Pkg (..), PkgName (..), pkgId)
import HWM.Domain.Bounds (deriveBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, Env (config), askWorkspaceGroups, updateConfig)
import HWM.Domain.Dependencies (Dependency (Dependency), lookupBounds, singleDeps)
import HWM.Domain.Matrix (getTestedRange)
import HWM.Domain.Workspace (resolveTargets)
import HWM.Integrations.Toolchain.Package
import HWM.Runtime.UI (putLine, section, sectionConfig, sectionTableM, sectionWorkspace)
import Options.Applicative (argument, help, long, metavar, short, str, strOption)
import Relude

data RegistryAddOptions = RegistryAddOptions {opsPkgName :: PkgName, opsWorkspace :: Maybe Text} deriving (Show)

instance ParseCLI RegistryAddOptions where
  parseCLI =
    RegistryAddOptions
      <$> argument (str >>= parse) (metavar "PACKAGE" <> help "Package name to add")
      <*> optional (strOption (long "workspace" <> short 'w' <> metavar "WORKSPACE" <> help "Target workspace ID"))

runRegistryAdd :: RegistryAddOptions -> ConfigT ()
runRegistryAdd RegistryAddOptions {opsPkgName, opsWorkspace} = do
  ws <- askWorkspaceGroups
  workspaces <- fmap (S.toList . S.fromList) (resolveTargets ws (maybeToList opsWorkspace))

  let target = if null workspaces then "none (registry only)" else format (T.intercalate ", " (map pkgId workspaces))
  sectionTableM
    0
    "add dependency"
    [ ("package", pure $ chalk Magenta (format opsPkgName)),
      ("target", pure $ chalk Cyan target)
    ]

  registered <- asks (lookupBounds opsPkgName . registry . config)
  case registered of
    Nothing -> do
      range <- getTestedRange
      section "discovery" $ do
        putLine $ padDots 16 "registry" <> "missing (initiating lookup)"

      bounds <- deriveBounds opsPkgName range
      let dependency = Dependency opsPkgName bounds

      ((\cf -> pure cf {registry = registry cf <> singleDeps dependency}) `updateConfig`) $ do
        sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
        addDepToPackage workspaces dependency
    Just bounds -> do
      section "discovery" $ do
        putLine $ padDots 16 "registry" <> format bounds <> " (already registered)"
      addDepToPackage workspaces (Dependency opsPkgName bounds)
  where
    addDepToPackage targets dependency =
      unless (null targets) $ sectionWorkspace $ do
        let maxLen = genMaxLen (map pkgMemberId targets)
        for_ targets $ \pkg -> updatePackage maxLen (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg
