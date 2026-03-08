{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build
  ( Builder (..),
    buildBinary,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import HWM.Core.Formatting (Format (..))
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue)
import HWM.Runtime.Process (exec)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, emptyPermissions, removeFile, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))

data Builder
  = CabalBuilder
  | StackBuilder
  | NixBuilder
  deriving (Generic, Show, Ord, Eq)

instance FromJSON Builder where
  parseJSON (String s) = parse s
  parseJSON _ = fail "Invalid builder. Expected 'cabal', 'stack', or 'nix'."

instance Parse Builder where
  parse "cabal" = pure CabalBuilder
  parse "stack" = pure StackBuilder
  parse "nix" = pure NixBuilder
  parse _ = fail "Invalid builder. Expected 'cabal', 'stack', or 'nix'."

instance ToJSON Builder where
  toJSON = String . format

instance Format Builder where
  format CabalBuilder = "cabal"
  format StackBuilder = "stack"
  format NixBuilder = "nix"

buildBinary :: (MonadError Issue m, MonadIO m) => Builder -> PkgName -> FilePath -> [Text] -> m ()
buildBinary builder pkgName dirPath args = do
  (success, buildOut) <- command
  unless success $ throwError (fromString $ "Build failed: " <> buildOut)
  when (builder == NixBuilder) (extractNixArtifact pkgName dirPath)
  where
    command = case builder of
      StackBuilder ->
        exec "stack" $ ["install", format pkgName, "--local-bin-path", format dirPath] <> args
      CabalBuilder ->
        exec "cabal"
          $ [ "install",
              format pkgName,
              "--install-method=copy",
              "--installdir",
              format dirPath,
              "--overwrite-policy=always"
            ]
          <> args
      NixBuilder ->
        -- WARNING: We DO NOT append 'args' here.
        -- Nix does not accept '--ghc-options' via CLI; it must be set in the flake.
        exec "nix" ["build", ".#" <> format pkgName, "-o", format (dirPath </> "result")]

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
    Nothing ->
      throwError
        $ fromString
        $ "Nix build succeeded, but binary '"
        <> pkgStr
        <> "' not found inside the Nix store path.\n"

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = do
  ifM (p x) (pure $ Just x) (findM p xs)
