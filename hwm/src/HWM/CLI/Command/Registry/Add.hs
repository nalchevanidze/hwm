{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Add (runRegistryAdd, RegistryAddOptions (..)) where

import qualified Data.Text as T
import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Core.Parsing (ParseCLI (..), parse, parseOptions)
import HWM.Core.Pkg (PkgName (..))
import HWM.Domain.Bounds (deriveBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (config), updateConfig)
import HWM.Domain.Dependencies (Dependency (Dependency))
import HWM.Domain.Environments (getTestedRange)
import HWM.Domain.Registry (addDependency, lookupBounds)
import HWM.Domain.Workspace (forWorkspaceTuple, resolveWorkspaces)
import HWM.Integrations.Toolchain.Package
import HWM.Runtime.UI (minRowSize, section, sectionTableM, uiRow)
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
    "add dependency"
    [ ("package", pure $ chalk Magenta (format opsPkgName)),
      ("target", pure $ chalk Cyan (if null opsWorkspace then "none (registry only)" else T.intercalate ", " opsWorkspace))
    ]
  registered <- asks (lookupBounds opsPkgName . fromMaybe mempty . cfgRegistry . config)
  case registered of
    Nothing -> do
      range <- getTestedRange
      section "discovery" $ do
        uiRow minRowSize "registry" "missing (initiating lookup)"

      bounds <- deriveBounds opsPkgName range
      let dependency = Dependency opsPkgName bounds

      ((\cf -> pure cf {cfgRegistry = Just $ addDependency dependency (fromMaybe mempty (cfgRegistry cf))}) `updateConfig`) $ addDepToPackage workspaces dependency
    Just bounds -> do
      section "discovery" $ do
        uiRow minRowSize "registry" (format bounds <> " (already registered)")
      addDepToPackage workspaces (Dependency opsPkgName bounds)
  where
    addDepToPackage ws dependency = unless (null ws) $ forWorkspaceTuple ws $ addPkgDependency dependency
