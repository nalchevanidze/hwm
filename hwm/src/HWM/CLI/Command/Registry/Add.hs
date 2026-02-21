{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Add (runRegistryAdd, RegistryAddOptions (..)) where

import qualified Data.Text as T
import HWM.Core.Formatting (Color (..), Format (..), chalk, padDots)
import HWM.Core.Parsing (ParseCLI (..), parse, parseOptions)
import HWM.Core.Pkg (PkgName (..))
import HWM.Domain.Bounds (deriveBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, Env (config), updateConfig)
import HWM.Domain.Dependencies (Dependency (Dependency), lookupBounds, singleDeps)
import HWM.Domain.Environments (getTestedRange)
import HWM.Domain.Workspace (forWorkspaceTuple, resolveWorkspaces)
import HWM.Integrations.Toolchain.Package
import HWM.Runtime.UI (putLine, section, sectionConfig, sectionTableM)
import Options.Applicative (argument, help, long, metavar, short, str)
import Relude

data RegistryAddOptions = RegistryAddOptions {opsPkgName :: PkgName, opsWorkspace :: [Text]} deriving (Show)

instance ParseCLI RegistryAddOptions where
  parseCLI =
    RegistryAddOptions
      <$> argument (str >>= parse) (metavar "PACKAGE" <> help "Package name to add")
      <*> parseOptions (long "workspace" <> short 'w' <> metavar "WORKSPACE" <> help "Target workspace ID")

runRegistryAdd :: RegistryAddOptions -> ConfigT ()
runRegistryAdd RegistryAddOptions {opsPkgName, opsWorkspace} = do
  workspaces <- resolveWorkspaces opsWorkspace
  sectionTableM
    0
    "add dependency"
    [ ("package", pure $ chalk Magenta (format opsPkgName)),
      ("target", pure $ chalk Cyan (if null opsWorkspace then "none (registry only)" else T.intercalate ", " opsWorkspace))
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
    addDepToPackage ws dependency =
      unless (null ws)
        $ forWorkspaceTuple ws
        $ \pkg -> updatePackage (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency))) pkg
