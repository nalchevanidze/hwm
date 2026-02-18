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

data RegistryAddOptions = RegistryAddOptions {regPkg :: PkgName, regTarget :: Maybe Text} deriving (Show)

instance ParseCLI RegistryAddOptions where
  parseCLI =
    RegistryAddOptions
      <$> argument (str >>= parse) (metavar "PACKAGE" <> help "Package name to add")
      <*> optional (strOption (long "workspace" <> short 'w' <> metavar "WORKSPACE" <> help "Target workspace ID"))

runRegistryAdd :: RegistryAddOptions -> ConfigT ()
runRegistryAdd RegistryAddOptions {regPkg, regTarget} = do
  let packageName = regPkg
      workspaceId = fromMaybe "default" regTarget

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
      range <- getTestedRange
      section "discovery" $ do
        putLine $ padDots 16 "registry" <> "missing (initiating lookup)"

      bounds <- deriveBounds packageName range
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
      sectionWorkspace $ do
        let maxLen = genMaxLen (map pkgMemberId targets)
        for_ targets $ \pkg -> updatePackage maxLen (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg