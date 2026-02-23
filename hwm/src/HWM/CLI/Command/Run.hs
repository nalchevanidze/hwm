{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Run
  ( runScript,
    ScriptOptions,
  )
where

import Control.Concurrent.Async
import Control.Monad.Error.Class (MonadError (..))
import Data.List (intersect)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable (for)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Format (..), Status (Checked, Invalid), chalk, genMaxLen, padDots, statusIcon)
import HWM.Core.Parsing (ParseCLI (..), parseOptions)
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment, getBuildEnvironments)
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (createEnvYaml, stackPath)
import HWM.Runtime.Logging (logIssue)
import HWM.Runtime.Process (inheritRun, silentRun)
import HWM.Runtime.UI (putLine, runSpinner, sectionEnvironments, sectionWorkspace, statusIndicator)
import Options.Applicative
  ( argument,
    help,
    long,
    metavar,
    short,
    str,
  )
import Relude

data ScriptOptions = ScriptOptions
  { scriptTargets :: [Name],
    scriptEnvs :: [Name],
    scriptOptions :: [Text]
  }
  deriving (Show)

instance ParseCLI ScriptOptions where
  parseCLI =
    ScriptOptions
      <$> parseOptions (long "target" <> short 't' <> metavar "TARGET" <> help "Limit to package (core) or group (libs)")
      <*> parseOptions (long "env" <> short 'e' <> metavar "ENV" <> help "Run in specific env (use 'all' for full matrix)")
      <*> many (argument str (metavar "ARGS..." <> help "Arguments to forward to the script"))

getEnvs :: [Name] -> ConfigT [BuildEnvironment]
getEnvs ["all"] = getBuildEnvironments
getEnvs names = for names (getBuildEnvironment . Just)

runScript :: Name -> ScriptOptions -> ConfigT ()
runScript scriptName ScriptOptions {..} = do
  cfg <- asks config
  case M.lookup scriptName (cfgScripts cfg) of
    Just script -> do
      envs <- getEnvs scriptEnvs
      targets <- concatMap snd <$> resolveWorkspaces scriptTargets
      for_ envs (createEnvYaml . buildName)
      let multi = length envs > 1
      let cmdTemplate = if null scriptOptions then script else T.unwords (script : scriptOptions)
      let padding = genMaxLen (map format envs)
      let run = runCommand padding multi cmdTemplate targets

      if multi
        then do
          when multi $ do
            sectionWorkspace $ do
              putLine $ padDots 16 "targets" <> if null scriptTargets then chalk Yellow "None (Global Scope)" else chalk Cyan (T.unwords scriptTargets)
            sectionEnvironments Nothing (for_ (map buildName envs) (run . Just))
        else run Nothing
    Nothing -> throwError $ fromString $ toString $ "Script not found: " <> scriptName

runCommand :: Int -> Bool -> Text -> [Pkg] -> Maybe Name -> ConfigT ()
runCommand padding multi scripts targets envName = do
  benv@BuildEnvironment {..} <- getBuildEnvironment envName
  let supported = targets `intersect` buildPkgs
  cmd <- resolveCommand scripts supported
  yamlPath <- stackPath envName
  if multi
    then do
      let env = format benv
      (success, content) <- silentRun yamlPath cmd (async (runSpinner padding env))
      statusIndicator padding env (statusIcon (if success then Checked else Invalid))
      unless success $ do
        path <- logIssue buildName SeverityError [("ENVIRONMENT", format benv), ("COMMAND", format cmd)] content
        throwError
          Issue
            { issueTopic = buildName,
              issueMessage = "Command failed",
              issueSeverity = SeverityError,
              issueDetails = Just CommandIssue {issueCommand = format cmd, issueLogFile = path}
            }
      putLine ""
    else do
      sectionWorkspace $ do
        putLine $ padDots 16 "targets" <> if null supported then chalk Yellow "None (Global Scope)" else chalk Cyan (T.unwords $ format . pkgName <$> supported)
      sectionEnvironments Nothing $ putLine $ format benv
      putLine ""
      putLine ("❯ " <> cmd)
      inheritRun yamlPath cmd
      putLine ""

resolveCommand :: Text -> [Pkg] -> ConfigT Text
resolveCommand cmd targets = do
  let hasPlaceholder = "{TARGET}" `T.isInfixOf` cmd
      hasTargets = not (null targets)
      targetsStr = T.unwords (map (format . pkgName) targets)
  let result = case (hasPlaceholder, hasTargets) of
        (True, True) -> Right $ T.replace "{TARGET}" targetsStr cmd
        (True, False) -> Left "Missing Target! This command requires specific targets (e.g. --target app1)."
        (False, True) -> Left "Target Not Allowed! This command is Global-only and does not support specific targets."
        (False, False) -> Right cmd
  case result of
    Left err -> throwError $ fromString err
    Right c -> pure c
