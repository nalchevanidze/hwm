{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Run
  ( runScript,
    ScriptOptions,
    TaskCommandOptions,
    runBuild,
    runInstall,
    runTest,
  )
where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..), parseOptions)
import HWM.Domain.Build (BuildFlag (..), BuilderCommand (..), TargetScope (..))
import HWM.Domain.Config (getScript)
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Dispatcher (DispatcheCommand (..), dispatchForEach)
import HWM.Domain.Environments (selectEnvironments)
import HWM.Domain.Workspace (printPkgWSRef, resolveWorkspaces)
import HWM.Runtime.Files (getLocalBinDir, warnBindDir)
import HWM.Runtime.Process (Exec (..), inheritRun)
import HWM.Runtime.UI (minRowSize, putLine, sectionWorkspace, uiRow, uiSubPath)
import Options.Applicative
  ( argument,
    help,
    long,
    metavar,
    short,
    str,
  )
import Options.Applicative.Builder (switch)
import Relude

newtype ScriptOptions = ScriptOptions {scriptOptions :: [Text]} deriving (Show)

instance ParseCLI ScriptOptions where
  parseCLI = ScriptOptions <$> many (argument str (metavar "ARGS..." <> help "Arguments to forward to the script"))

runScript :: Name -> ScriptOptions -> ConfigT ()
runScript scriptName ScriptOptions {..} = do
  cfg <- asks config
  script <- getScript scriptName cfg
  putLine ("❯ " <> script)
  inheritRun Exec {execCmd = script, execArgs = scriptOptions, execEnv = [], postCommand = Nothing}

data TaskCommandOptions = TaskCommandOptions
  { opsEnviroments :: [Name],
    opsWorkspaces :: [Name],
    opsFast :: Bool
  }
  deriving (Show)

instance ParseCLI TaskCommandOptions where
  parseCLI =
    TaskCommandOptions
      <$> parseOptions (long "env" <> short 'e' <> metavar "ENV" <> help "Run in specific env (use 'all' for full matrix)")
      <*> many (argument str (metavar "WORKSPACE" <> help "Limit to package (core) or group (libs)"))
      <*> switch (long "fast" <> help "Enable fast mode")

parseTargets :: [Name] -> ConfigT TargetScope
parseTargets names = case names of
  [] -> do
    sectionWorkspace $ uiRow minRowSize "scope" "Global"
    pure ScopeGlobal
  _ -> do
    pkgs <- concatMap snd <$> resolveWorkspaces names
    sectionWorkspace $ for_ pkgs $ \pkg -> uiSubPath (printPkgWSRef pkg)
    pure $ ScopePkgs pkgs

runBuild :: TaskCommandOptions -> ConfigT ()
runBuild TaskCommandOptions {..} = do
  scope <- parseTargets opsWorkspaces
  envs <- selectEnvironments opsEnviroments
  dispatchForEach (DispatcheCommand Build scope [BuildFastFlag | opsFast]) envs

runInstall :: TaskCommandOptions -> ConfigT ()
runInstall TaskCommandOptions {..} = do
  scope <- parseTargets opsWorkspaces
  envs <- selectEnvironments opsEnviroments
  binDir <- getLocalBinDir
  warnBindDir binDir
  dispatchForEach (DispatcheCommand (Install binDir) scope [BuildFastFlag | opsFast]) envs

runTest :: TaskCommandOptions -> ConfigT ()
runTest TaskCommandOptions {..} = do
  scope <- parseTargets opsWorkspaces
  envs <- selectEnvironments opsEnviroments
  dispatchForEach (DispatcheCommand Test scope [BuildFastFlag | opsFast]) envs
