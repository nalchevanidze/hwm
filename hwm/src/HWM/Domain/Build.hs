{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build
  ( Builder (..),
    BuilderCommand (..),
    TargetScope (..),
    toExec,
    comandLabel,
    isCustom,
    BuildFlag (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import qualified Data.Text as T
import HWM.Core.Formatting (Format (..))
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue)
import HWM.Runtime.Platform (Platform (..), detectPlatform, toNixSystem)
import HWM.Runtime.Process (EnvVars, Exec (..), mkExec)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, emptyPermissions, listDirectory, removeFile, removePathForcibly, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))

data Builder
  = CabalBuilder {inNixDevelopment :: Bool}
  | StackBuilder
  | NixBuilder
  deriving (Generic, Show, Ord, Eq)

instance FromJSON Builder where
  parseJSON (String s) = parse s
  parseJSON _ = fail "Invalid builder. Expected 'cabal', 'stack', or 'nix'."

instance Parse Builder where
  parse "cabal" = pure CabalBuilder {inNixDevelopment = False}
  parse "nix/cabal" = pure CabalBuilder {inNixDevelopment = True}
  parse "stack" = pure StackBuilder
  parse "nix" = pure NixBuilder
  parse _ = fail "Invalid builder. Expected 'cabal', 'stack', or 'nix'."

instance ToJSON Builder where
  toJSON = String . format

instance Format Builder where
  format (CabalBuilder False) = "cabal"
  format (CabalBuilder True) = "nix/cabal"
  format StackBuilder = "stack"
  format NixBuilder = "nix"

data Cmd = ExecCmd Text [Text] EnvVars | CusomCmd Text
  deriving (Eq, Show)

comandLabel :: BuilderCommand -> Text
comandLabel Build {} = "build"
comandLabel Test {} = "test"
comandLabel Install {} = "build"
comandLabel Custom {} = "comand"

extractNixArtifact :: (MonadIO m, MonadError Issue m) => PkgName -> FilePath -> m ()
extractNixArtifact pkgName distDir = do
  let resultLink = distDir </> "result"
      finalDest = distDir </> toString pkgName
      pkgStr = toString (format pkgName)

  liftIO $ createDirectoryIfMissing True distDir
  isLink <- liftIO $ doesPathExist resultLink
  unless isLink
    $ throwError
    $ fromString
    $ "Nix build completed, but did not create an output at: "
    <> resultLink
    <> "\n(This usually means the Nix derivation is empty or 'exec' hid a build failure.)"
  let searchPaths =
        [ resultLink </> "bin" </> pkgStr, -- Standard Haskell (Cabal/Stack)
          resultLink </> pkgStr, -- Simple/Single-binary derivation
          resultLink -- Derivation is the binary itself
        ]
  maybeSource <- findM (liftIO . doesFileExist) searchPaths

  case maybeSource of
    Just sourcePath -> do
      liftIO $ copyFile sourcePath finalDest
      -- Ensure the user can execute it (Nix store is read-only)
      liftIO $ do
        let properPerms =
              setOwnerReadable True
                $ setOwnerWritable True
                $ setOwnerExecutable True emptyPermissions
        setPermissions finalDest properPerms
      -- Cleanup: Remove the 'result' symlink to keep the folder clean
      liftIO $ removeFile resultLink
    Nothing -> throwError $ fromString $ "Nix build succeeded, but binary '" <> pkgStr <> "' not found inside the Nix store path.\n"

extractGlobalNixArtifacts :: (MonadIO m, MonadError Issue m) => FilePath -> m ()
extractGlobalNixArtifacts distDir = do
  let resultLink = distDir </> "result-global"
      binDir = resultLink </> "bin"

  isLink <- liftIO $ doesPathExist resultLink
  unless isLink $ throwError "Nix global build failed to produce a result symlink."

  hasBin <- liftIO $ doesPathExist binDir
  if hasBin
    then do
      -- Copy everything inside result-global/bin/
      files <- liftIO $ listDirectory binDir
      for_ files $ \file -> do
        let sourcePath = binDir </> file
        let destPath = distDir </> file
        liftIO $ copyFile sourcePath destPath
    -- (Apply the same executable permissions here as before)
    else
      -- Fallback if the default package is just a single binary at the root
      throwError "Global Nix install succeeded, but no 'bin/' directory was found in the output."

  -- Clean up the symlink
  liftIO $ removePathForcibly resultLink

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = ifM (p x) (pure $ Just x) (findM p xs)

data TargetScope
  = ScopeGlobal -- User typed: hwm build (Build everything)
  | ScopePkgs [Pkg] -- User typed: hwm build -w=libs/... (Build these)
  deriving (Eq, Show)

isCustom :: BuilderCommand -> Bool
isCustom Custom {} = True
isCustom _ = False

data BuilderCommand
  = Build
  | Test
  | Install {dirPath :: FilePath}
  | Custom Text
  deriving (Eq, Show)

data BuildFlag
  = CustomBuildFlag Text
  | BuildFastFlag
  | GHCOptionsFlag Text
  deriving (Eq, Show)

formatFlag :: Builder -> BuildFlag -> [Text]
-- WARNING: Nix does not accept '--ghc-options' via CLI; it must be set in the flake.
formatFlag NixBuilder _ = []
formatFlag CabalBuilder {} BuildFastFlag = ["--disable-optimization"]
formatFlag StackBuilder BuildFastFlag = ["--fast"]
formatFlag _ (GHCOptionsFlag xs) = ["--ghc-options=" <> xs]
formatFlag _ (CustomBuildFlag txt) = [txt]

toExec :: (MonadIO m, MonadError Issue m) => Builder -> BuilderCommand -> TargetScope -> [BuildFlag] -> [(String, String)] -> m (Exec m)
toExec builder cmd scope flags envs = do
  p <- detectPlatform
  Exec {..} <- toAction p builder cmd scope
  pure $ inNixDevelop nixEnabled $ Exec execCmd (execArgs <> concatMap (formatFlag builder) flags) envs postCommand
  where
    nixEnabled = CabalBuilder {inNixDevelopment = True} == builder

handleScope :: TargetScope -> [Text]
handleScope ScopeGlobal = []
handleScope (ScopePkgs pkgs) = map (format . pkgName) pkgs

inNixDevelop :: Bool -> Exec m -> Exec m
inNixDevelop True (Exec cmd ops env post) = Exec "nix" (["develop", "--command", cmd] <> ops) env post
inNixDevelop False e = e

toAction :: (MonadError Issue m, MonadIO m) => Platform -> Builder -> BuilderCommand -> TargetScope -> m (Exec m)
-- Stack and Cabal ignore the system string
toAction _ StackBuilder Build scope = mkExec "stack" (["build"] <> handleScope scope)
toAction _ CabalBuilder {} Build ScopeGlobal = mkExec "cabal" ["build", "all"]
toAction _ CabalBuilder {} Build (ScopePkgs pkgs) = mkExec "cabal" (["build"] <> handleScope (ScopePkgs pkgs))
toAction _ StackBuilder Install {..} scope = mkExec "stack" (["install"] <> handleScope scope <> ["--local-bin-path", format dirPath])
toAction _ CabalBuilder {} Install {..} ScopeGlobal = mkExec "cabal" (["install", "all:exes"] <> ["--install-method=copy", "--installdir", format dirPath, "--overwrite-policy=always"])
toAction _ CabalBuilder {} Install {..} scope = mkExec "cabal" (["install"] <> handleScope scope <> ["--install-method=copy", "--installdir", format dirPath, "--overwrite-policy=always"])
toAction _ StackBuilder Test scope = mkExec "stack" (["test"] <> handleScope scope)
toAction _ CabalBuilder {} Test ScopeGlobal = mkExec "cabal" ["test", "all"]
toAction _ CabalBuilder {} Test scope = mkExec "cabal" (["test"] <> handleScope scope)
-- Nix uses the system string
toAction _ NixBuilder Build ScopeGlobal = mkExec "nix" ["build"]
toAction _ NixBuilder Build (ScopePkgs pkgs) = mkExec "nix" $ ["build"] <> map (\pkg -> ".#" <> format (pkgName pkg)) pkgs
-- Start Nix Install
toAction _ NixBuilder Install {..} ScopeGlobal = pure $ Exec "nix" ["build", ".#", "-o", format (dirPath </> "result-global")] [] (Just $ extractGlobalNixArtifacts dirPath)
toAction _ NixBuilder Install {..} (ScopePkgs [pkg]) =
  pure $ Exec "nix" ["build", ".#" <> format (pkgName pkg), "-o", format (dirPath </> "result")] [] (Just $ extractNixArtifact (pkgName pkg) dirPath)
toAction _ NixBuilder Install {} (ScopePkgs _) = throwError "Multiple package install is not supported with Nix builder."
-- end Nix Install
toAction _ NixBuilder Test ScopeGlobal = mkExec "nix" ["flake", "check"]
-- Map over the list of packages (ac) to build multiple test checks at once!
toAction p NixBuilder Test (ScopePkgs pkgs) =
  mkExec "nix"
    $ ["build", "-L", "--no-link"]
    <> map (\pkg -> ".#checks." <> toNixSystem p <> "." <> format (pkgName pkg)) pkgs
toAction _ _ (Custom customCmd) scope = do
  cmd <- resolveTargets scope customCmd
  mkExec cmd []

resolveTargets :: (MonadError Issue m) => TargetScope -> Text -> m Text
resolveTargets ScopeGlobal cmd
  | hasTarget cmd = throwError "Target Not Allowed! This command is Global-only and does not support specific targets."
  | otherwise = pure cmd
resolveTargets (ScopePkgs pkgs) cmd
  | hasTarget cmd = pure $ T.replace targetKeyword (T.unwords (map (format . pkgName) pkgs)) cmd
  | otherwise = throwError "Missing Target! This command requires specific targets (e.g. --target app1)."

hasTarget :: Text -> Bool
hasTarget cmd = targetKeyword `T.isInfixOf` cmd

targetKeyword :: Text
targetKeyword = "{TARGET}"
