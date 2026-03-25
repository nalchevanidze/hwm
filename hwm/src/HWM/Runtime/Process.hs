{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Process
  ( inheritRun,
    exec,
    Exec (..),
    ExecOptions (..),
    EnvVars,
    execInBackground,
    mkExec,
  )
where

import Control.Concurrent.Async
import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Status (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Runtime.Files (prepareDir)
import HWM.Runtime.Logging (genLogId, logCommandEnd, logCommandStart, logPath, logRoot)
import HWM.Runtime.Platform (OS (..), Platform (..), detectPlatform)
import HWM.Runtime.UI
import Relude
import System.Environment (getEnvironment)
import qualified System.Environment as Env
import qualified System.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Process.Typed
  ( ExitCode (..),
    Process,
    proc,
    runProcess_,
    setEnv,
    setStderr,
    setStdout,
    shell,
    useHandleOpen,
    waitExitCode,
    withProcessWait,
  )

mkExec :: (Applicative m) => Text -> [Text] -> m (Exec m)
mkExec name args = pure $ Exec name args [] Nothing

exec :: (MonadIO m) => Text -> [Text] -> m (Bool, String)
exec name args = do
  (code, _, out) <- liftIO (readProcessWithExitCode (toString name) (map toString args) "")
  case code of
    ExitSuccess {} -> pure (True, out)
    ExitFailure {} -> pure (False, out)

data Exec m = Exec
  { execCmd :: Text,
    execArgs :: [Text],
    execEnv :: [(String, String)],
    postCommand :: Maybe (m ())
  }

type EnvVars = [(String, String)]

data ExecOptions = ExecOptions
  { envName :: Name,
    formatFX :: Text -> Text -> Text,
    fxEnabled :: Bool
  }

processHandle :: IO a -> Bool -> Handle -> Process stdin stdout stderr -> IO ExitCode
processHandle f True logHandle p = do
  spinner <- async f
  status <- waitExitCode p
  cancel spinner
  logCommandEnd logHandle status
  pure status
processHandle _ False logHandle p = do
  status <- waitExitCode p
  logCommandEnd logHandle status
  pure status

execInBackground :: (MonadUI m, MonadError Issue m, MonadIO m) => Exec m -> ExecOptions -> m ()
execInBackground Exec {..} ExecOptions {..} = do
  logId <- genLogId envName
  let fx = formatFX (toText $ logPath logId)
  let processLogPath = logPath logId
  prepareDir logRoot
  let cmd = execCmd <> " " <> T.unwords execArgs
  currentEnv <- liftIO getEnvironment
  let targetEnv = execEnv <> currentEnv
  status <- liftIO $ TIO.withFile processLogPath TIO.WriteMode $ \logHandle -> do
    unless fxEnabled (uiIndicator fx fxEnabled Nothing)
    logCommandStart logHandle cmd
    let config = setEnv targetEnv $ setStdout (useHandleOpen logHandle) $ setStderr (useHandleOpen logHandle) $ shell (toString cmd)
    withProcessWait config $ processHandle (uiIndicator fx fxEnabled Nothing) fxEnabled logHandle
  case status of
    ExitSuccess -> do
      liftIO $ uiIndicator fx fxEnabled (Just Checked)
      sequence_ postCommand
    _ -> do
      liftIO $ uiIndicator fx fxEnabled (Just Invalid)
      throwError
        Issue
          { issueTopic = logId,
            issueMessage = "Command failed",
            issueSeverity = SeverityError,
            issueDetails = Just CommandIssue {issueCommand = cmd, issueLogFile = processLogPath}
          }

data ShellSpec = ShellSpec
  { shellCmd :: FilePath,
    shellPrefixArgs :: [String]
  }

resolveShell :: IO ShellSpec
resolveShell = do
  platform <- detectPlatform
  case os platform of
    Windows -> do
      cmd <- fromMaybe "cmd.exe" <$> Env.lookupEnv "COMSPEC"
      pure ShellSpec {shellCmd = cmd, shellPrefixArgs = ["/d", "/s", "/c"]}
    _ -> do
      sh <- fromMaybe "sh" <$> Env.lookupEnv "SHELL"
      pure ShellSpec {shellCmd = sh, shellPrefixArgs = ["-c"]}

inheritRun :: (MonadIO m, MonadUI m) => Exec m -> m ()
inheritRun Exec {execCmd = cmd, execEnv = env, postCommand = post} = do
  currentEnv <- liftIO getEnvironment
  let targetEnv = env <> currentEnv
  ShellSpec {..} <- liftIO resolveShell
  let processConfig = setEnv targetEnv $ proc shellCmd (shellPrefixArgs <> [toString cmd])
  liftIO (runProcess_ processConfig)
  sequence_ post
