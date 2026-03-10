{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build
  ( Builder (..),
    BuilderCommand (..),
    runBuilderCommand,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import qualified Data.Text as T
import HWM.Core.Formatting (Format (..))
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue)
import HWM.Runtime.Platform (Platform (..), detectPlatform, toNixSystem)
import HWM.Runtime.Process (Exec (..), runInBackground)
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

runBuilderCommand :: (MonadError Issue m, MonadIO m) => Builder -> Bool -> BuilderCommand -> [Text] -> m ()
runBuilderCommand builder nixEnabled cmd args = do
  p <- detectPlatform
  let action = toAction p builder cmd
  let Exec {..} = if nixEnabled && builder /= NixBuilder then inNixDevelop action else action
  -- WARNING: Nix does not accept '--ghc-options' via CLI; it must be set in the flake.
  let exec = Exec execCmd (execArgs <> if builder == NixBuilder then [] else args) []
  runInBackground exec "build" 16
  postAction builder cmd

postAction :: (MonadIO m, MonadError Issue m) => Builder -> BuilderCommand -> m ()
postAction NixBuilder Install {..} = extractNixArtifact iName dirPath
postAction _ _ = pure ()

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

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = do
  ifM (p x) (pure $ Just x) (findM p xs)

data BuilderCommand = Build [PkgName] | Test [PkgName] | Install {iName :: PkgName, dirPath :: FilePath}
  deriving (Eq, Show)

instance Format BuilderCommand where
  format (Build pkgs) = "build " <> T.unwords (map format pkgs)
  format (Test pkgs) = "test " <> T.unwords (map format pkgs)
  format (Install pkg dir) = "install " <> format pkg <> " to " <> toText dir

mkExec :: Text -> [Text] -> Exec
mkExec name args = Exec name args []

toAction :: Platform -> Builder -> BuilderCommand -> Exec
-- Stack and Cabal ignore the system string
toAction _ StackBuilder (Build pkgs) = mkExec "stack" (["build"] <> map format pkgs)
toAction _ CabalBuilder (Build pkgs) = mkExec "cabal" (["build"] <> map format pkgs)
toAction _ StackBuilder Install {..} = mkExec "stack" ["install", format iName, "--local-bin-path", format dirPath]
toAction _ CabalBuilder Install {..} = mkExec "cabal" ["install", format iName, "--install-method=copy", "--installdir", format dirPath, "--overwrite-policy=always"]
toAction _ StackBuilder (Test ac) = mkExec "stack" $ ["test"] <> map format ac
toAction _ CabalBuilder (Test ac) = mkExec "cabal" $ ["test"] <> map format ac
-- Nix uses the system string
toAction _ NixBuilder (Build pkgs) = mkExec "nix" $ ["build"] <> map (\pkg -> ".#" <> format pkg) pkgs
toAction _ NixBuilder Install {..} = mkExec "nix" ["build", ".#" <> format iName, "-o", format (dirPath </> "result")]
toAction _ NixBuilder (Test []) = mkExec "nix" ["flake", "check"]
-- Map over the list of packages (ac) to build multiple test checks at once!
toAction p NixBuilder (Test ac) =
  mkExec "nix"
    $ ["build", "-L", "--no-link"]
    <> map (\pkg -> ".#checks." <> toNixSystem p <> "." <> format pkg) ac

inNixDevelop :: Exec -> Exec
inNixDevelop (Exec cmd ops env) = Exec "nix" (["develop", "--command", cmd] <> ops) env
