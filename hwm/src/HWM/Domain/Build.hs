{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build
  ( Builder (..),
    BuilderCommand (..),
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
import HWM.Core.Formatting (Format (..))
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue)
import HWM.Domain.Schema (TargetScope (..))
import HWM.Integrations.Toolchain.Nix.Build (NixEnv (..), genNixBinary, inNixDevelop, nixBuild)
import HWM.Runtime.Process (EnvVars, Exec (..), mkExec)
import Relude

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
  parse _ = fail "Invalid builder. Expected 'cabal', 'stack', 'nix', or 'nix/cabal'."

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
-- Nix does not accept '--ghc-options' via CLI; it must be set in the flake.
formatFlag NixBuilder _ = []
formatFlag CabalBuilder {} BuildFastFlag = ["--disable-optimization"]
formatFlag StackBuilder BuildFastFlag = ["--fast"]
formatFlag _ (GHCOptionsFlag xs) = ["--ghc-options=" <> xs]
formatFlag _ (CustomBuildFlag txt) = [txt]

toExec :: (MonadIO m, MonadError Issue m) => Name -> Builder -> BuilderCommand -> TargetScope -> [BuildFlag] -> [(String, String)] -> m (Exec m)
toExec envName builder cmd scope flags envs = do
  Exec {..} <- toAction (NixEnv envName) builder cmd scope
  pure $ inNixDevelop envName nixEnabled $ Exec execCmd (execArgs <> concatMap (formatFlag builder) flags) envs postCommand
  where
    nixEnabled = CabalBuilder {inNixDevelopment = True} == builder

handleScope :: Maybe Text -> TargetScope -> [Text]
handleScope fb ScopeGlobal = maybeToList fb
handleScope _ (ScopePkgs pkgs) = map (format . pkgName) pkgs

mkStack :: (Applicative m) => Text -> TargetScope -> [Text] -> m (Exec m)
mkStack cmd scope ops = mkExec "stack" ([cmd] <> handleScope Nothing scope <> ops)

mkCabal :: (Applicative m) => Bool -> Text -> TargetScope -> [Text] -> m (Exec m)
mkCabal install cmd scope ops = mkExec "cabal" ([cmd] <> handleScope (if install then Just "all:exes" else Just "all") scope <> ops)

installCabal :: (Applicative m, Format a) => TargetScope -> a -> m (Exec m)
installCabal scope dirPath = mkCabal True "install" scope ["--install-method=copy", "--installdir", format dirPath, "--overwrite-policy=always"]

installStack :: (Applicative m, Format a) => TargetScope -> a -> m (Exec m)
installStack scope dirPath = mkStack "install" scope ["--local-bin-path", format dirPath]

toAction :: (MonadError Issue m, MonadIO m) => NixEnv -> Builder -> BuilderCommand -> TargetScope -> m (Exec m)
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
toAction ctx NixBuilder BuildArtifact {..} scope = genNixBinary ctx scope dirPath
toAction _ StackBuilder BuildArtifact {..} scope = installStack scope dirPath
toAction _ CabalBuilder {} BuildArtifact {..} scope = installCabal scope dirPath
