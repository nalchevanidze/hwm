{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build
  ( Builder (..),
    BuilderCommand (..),
    TargetScope (..),
    BuildFlag (..),
    toExec,
    comandLabel,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), toCamelCase)
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue)
import HWM.Runtime.Process (EnvVars, Exec (..), mkExec)
import Relude
import System.Directory (copyFile, doesFileExist, doesPathExist, emptyPermissions, removePathForcibly, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
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
comandLabel BuildArtifact {} = "build"

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
extractNixArtifact pkgName dir = forNixLink dir $ \resultLink -> do
  let pkgStr = toString (format pkgName)
  let searchPaths = [resultLink </> "bin" </> pkgStr, resultLink </> pkgStr, resultLink]
  source <- findM (liftIO . doesFileExist) searchPaths
  case source of
    Just path -> copyBinary path (dir </> toString pkgName)
    Nothing -> throwError $ fromString $ "Nix build succeeded, but binary '" <> pkgStr <> "' not found inside the Nix store path.\n"

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = ifM (p x) (pure $ Just x) (findM p xs)

data TargetScope
  = ScopeGlobal -- User typed: hwm build (Build everything)
  | ScopePkgs [Pkg] -- User typed: hwm build -w=libs/... (Build these)
  deriving (Eq, Show)

data BuilderCommand
  = Build
  | Test
  | Install {dirPath :: FilePath}
  | BuildArtifact {dirPath :: FilePath}
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

toExec :: (MonadIO m, MonadError Issue m) => Name -> Builder -> BuilderCommand -> TargetScope -> [BuildFlag] -> [(String, String)] -> m (Exec m)
toExec envName builder cmd scope flags envs = do
  Exec {..} <- toAction (Env envName) builder cmd scope
  pure $ inNixDevelop envName nixEnabled $ Exec execCmd (execArgs <> concatMap (formatFlag builder) flags) envs postCommand
  where
    nixEnabled = CabalBuilder {inNixDevelopment = True} == builder

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

handleScope :: Maybe Text -> TargetScope -> [Text]
handleScope fb ScopeGlobal = maybeToList fb
handleScope _ (ScopePkgs pkgs) = map (format . pkgName) pkgs

mkStack :: (Applicative m) => Text -> TargetScope -> [Text] -> m (Exec m)
mkStack cmd scope ops = mkExec "stack" ([cmd] <> handleScope Nothing scope <> ops)

mkCabal :: (Applicative m) => Bool -> Text -> TargetScope -> [Text] -> m (Exec m)
mkCabal install cmd scope ops = mkExec "cabal" ([cmd] <> handleScope (if install then Just "all:exes" else Just "all") scope <> ops)

newtype Env = Env {envName :: Name}

nixBuild :: (Applicative m) => Env -> TargetScope -> m (Exec m)
nixBuild ctx scope = mkExec "nix" $ ["build", "--no-link"] <> nixScope (envName ctx) scope

installCabal :: (Applicative m, Format a) => TargetScope -> a -> m (Exec m)
installCabal scope dirPath = mkCabal True "install" scope ["--install-method=copy", "--installdir", format dirPath, "--overwrite-policy=always"]

installStack :: (Applicative m, Format a) => TargetScope -> a -> m (Exec m)
installStack scope dirPath = mkStack "install" scope ["--local-bin-path", format dirPath]

buildNixArtifact :: FilePath -> [Text] -> m () -> Exec m
buildNixArtifact dirPath scope m = Exec "nix" (["build"] <> scope <> ["-o", format (dirPath </> "result")]) [] (Just m)

getNixtScope :: (MonadError Issue m, MonadIO m) => TargetScope -> m PkgName
getNixtScope (ScopePkgs [pkg]) = pure $ pkgName pkg
getNixtScope _ = throwError "BuildArtifact command with Nix builder is only supported for a single package"

toAction :: (MonadError Issue m, MonadIO m) => Env -> Builder -> BuilderCommand -> TargetScope -> m (Exec m)
-- Stack
toAction _ StackBuilder Build scope = mkStack "build" scope []
toAction _ StackBuilder Test scope = mkStack "test" scope []
-- Cabal
toAction _ CabalBuilder {} Build scope = mkCabal False "build" scope []
toAction _ CabalBuilder {} Test scope = mkCabal False "test" scope []
-- Nix
toAction ctx NixBuilder Build scope = nixBuild ctx scope
toAction ctx NixBuilder Test scope = nixBuild ctx scope
-- INSTALL
toAction _ CabalBuilder {..} Install {..} scope
  | inNixDevelopment = throwError "Install command with Nix development environment is not supported"
  | otherwise = installCabal scope dirPath
toAction _ StackBuilder Install {..} scope = installStack scope dirPath
toAction _ NixBuilder Install {} _ = throwError "Install command with Nix builder is not supported"
--
toAction _ NixBuilder BuildArtifact {..} scope = do
  pkgName <- getNixtScope scope
  pure $ buildNixArtifact dirPath [".#" <> format pkgName] (extractNixArtifact pkgName dirPath)
toAction _ StackBuilder BuildArtifact {..} scope = installStack scope dirPath
toAction _ CabalBuilder {} BuildArtifact {..} scope = installCabal scope dirPath