{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Nix.Build
  ( genNixBinary,
    nixBuild,
    inNixDevelop,
    NixEnv (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), toCamelCase)
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue)
import HWM.Domain.Schema (TargetScope (..))
import HWM.Runtime.Process (Exec (..), mkExec)
import Relude
import System.Directory (copyFile, doesFileExist, doesPathExist, emptyPermissions, removePathForcibly, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))

forNixLink :: (MonadIO m, MonadError Issue m) => FilePath -> (FilePath -> m ()) -> m ()
forNixLink dir f = do
  let linkPath = dir </> "result"
  isLink <- liftIO $ doesPathExist linkPath
  unless isLink
    $ throwError
    $ fromString
      ( "Nix build completed, but did not create an output at: "
          <> linkPath
          <> "\n(This usually means the Nix derivation is empty or build failed silently.)"
      )
  f linkPath
  liftIO $ removePathForcibly linkPath

copyBinary :: (MonadIO m) => FilePath -> FilePath -> m ()
copyBinary fromPath toPath = liftIO $ do
  liftIO $ copyFile fromPath toPath
  let properPerms =
        setOwnerReadable True
          $ setOwnerWritable True
          $ setOwnerExecutable True emptyPermissions
  setPermissions toPath properPerms

extractNixArtifact :: (MonadIO m, MonadError Issue m) => PkgName -> FilePath -> m ()
extractNixArtifact pkgName dir = forNixLink dir $ \link -> do
  let bin = link </> "bin" </> toString pkgName
  exists <- liftIO $ doesFileExist bin
  if exists
    then copyBinary bin (dir </> toString pkgName)
    else throwError $ fromString $ "Nix build succeeded, but binary '" <> bin <> "' not found inside the Nix store path.\n"

toNixEnv :: Text -> Text
toNixEnv name = ".#" <> toCamelCase name

inNixDevelop :: Name -> Bool -> Exec m -> Exec m
inNixDevelop envName True (Exec cmd ops env post) = Exec "nix" (["develop", toNixEnv envName, "--command", cmd] <> ops) env post
inNixDevelop _ False e = e

nixScope :: (Format p) => p -> TargetScope -> [Text]
nixScope envName ScopeGlobal =
  let envSuffix = toCamelCase (format envName)
   in [".#env-" <> envSuffix <> "-all"]
nixScope envName (ScopePkgs pkgs) =
  let envSuffix = toCamelCase (format envName)
   in map (\pkg -> ".#" <> format (pkgName pkg) <> "-" <> envSuffix) pkgs

newtype NixEnv = NixEnv {envName :: Name}

nixBuild :: (Applicative m) => NixEnv -> TargetScope -> m (Exec m)
nixBuild ctx scope = mkExec "nix" $ ["build", "--no-link"] <> nixScope (envName ctx) scope

getNixtScope :: (MonadError Issue m, MonadIO m) => TargetScope -> m PkgName
getNixtScope (ScopePkgs [pkg]) = pure $ pkgName pkg
getNixtScope _ = throwError "BuildArtifact command with Nix builder is only supported for a single package"

buildNixArtifact :: NixEnv -> FilePath -> PkgName -> m () -> Exec m
buildNixArtifact NixEnv {..} dirPath scope m = Exec "nix" ["build", ".#" <> format scope <> "-" <> format envName <> "-release", "-o", format (dirPath </> "result")] [] (Just m)

genNixBinary :: (MonadIO m, MonadError Issue m, MonadError Issue m) => NixEnv -> TargetScope -> FilePath -> m (Exec m)
genNixBinary ctx scope dirPath = do
  pkgName <- getNixtScope scope
  pure $ buildNixArtifact ctx dirPath pkgName (extractNixArtifact pkgName dirPath)
