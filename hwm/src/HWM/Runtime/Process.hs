{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Process
  ( inheritRun,
    exec,
    execInBackground,
    Exec (..),
    ExecOptions (..),
    EnvVars,
  )
where

import Control.Concurrent.Async
import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (Dim), Status (..), chalk, indentBlockNum, padDots)
import HWM.Core.Options (isCI)
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Runtime.Files (prepareDir)
import HWM.Runtime.Logging (genLogId, logCommandEnd, logCommandStart, logPath, logRoot)
import HWM.Runtime.UI (MonadUI (uiIndentLevel), uiIndicator)
import Relude
import System.Environment (getEnvironment)
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

exec :: (MonadIO m) => Text -> [Text] -> m (Bool, String)
exec name args = do
  (code, _, out) <- liftIO (readProcessWithExitCode (toString name) (map toString args) "")
  case code of
    ExitSuccess {} -> pure (True, out)
    ExitFailure {} -> pure (False, out)

data Exec = Exec
  { execCmd :: Text,
    execArgs :: [Text],
    execEnv :: [(String, String)]
  }

type EnvVars = [(String, String)]

data ExecOptions = ExecOptions
  { logId :: Name,
    loopIO :: Bool -> Maybe Status -> IO (),
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

execAsync :: (MonadUI m, MonadIO m) => Exec -> ExecOptions -> m [Issue]
execAsync Exec {..} ExecOptions {..} = do
  let processLogPath = logPath logId
  prepareDir logRoot
  let cmd = execCmd <> " " <> T.unwords execArgs
  currentEnv <- liftIO getEnvironment

  let targetEnv = execEnv <> currentEnv
  liftIO $ do
    status <- TIO.withFile processLogPath TIO.WriteMode $ \logHandle -> do
      logCommandStart logHandle cmd
      let processConfig =
            setEnv targetEnv
              $ setStdout (useHandleOpen logHandle)
              $ setStderr (useHandleOpen logHandle)
              $ shell (toString cmd)
      withProcessWait processConfig $ processHandle (loopIO fxEnabled Nothing) fxEnabled logHandle
    case status of
      ExitSuccess -> do
        liftIO $ loopIO fxEnabled (Just Checked)
        pure []
      _ -> do
        liftIO $ loopIO fxEnabled (Just Invalid)
        pure
          [ Issue
              { issueTopic = logId,
                issueMessage = "Command failed",
                issueSeverity = SeverityError,
                issueDetails = Just CommandIssue {issueCommand = cmd, issueLogFile = processLogPath}
              }
          ]

inheritRun :: (MonadIO m, MonadUI m) => Exec -> m ()
inheritRun Exec {..} = do
  currentEnv <- liftIO getEnvironment
  let targetEnv = execEnv <> currentEnv
  let processConfig = setEnv targetEnv $ proc "/bin/sh" (["-c", toString execCmd] <> map toString execArgs)
  liftIO (runProcess_ processConfig)

inNixDevelop :: Bool -> Exec -> Exec
inNixDevelop True (Exec cmd ops env) = Exec "nix" (["develop", "--command", cmd] <> ops) env
inNixDevelop False e = e

execInBackground :: (MonadIO m, MonadUI m, MonadError Issue m) => Bool -> Exec -> Name -> Name -> Int -> m ()
execInBackground useNix e label env padding = do
  fxEnabled <- not <$> isCI
  logId <- genLogId env
  ind <- uiIndentLevel
  let logsSuffix = chalk Dim (" logs: " <> toText (logPath logId))
  let f icon = indentBlockNum ind (padDots padding label <> icon <> logsSuffix)
  issues <- execAsync (inNixDevelop useNix e) ExecOptions {logId = logId, loopIO = uiIndicator f, fxEnabled = fxEnabled}
  traverse_ throwError issues
