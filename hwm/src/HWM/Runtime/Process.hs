{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Process
  ( silentRun,
    inheritRun,
  )
where

import Control.Concurrent.Async
import qualified Data.Text as T
import GHC.IO (evaluate)
import Relude
import System.Environment (getEnvironment)
import qualified System.IO as TIO
import System.Process.Typed

provideYamlPath :: (MonadIO m) => String -> m [(String, String)]
provideYamlPath yamlPath = do
  currentEnv <- liftIO getEnvironment
  pure (("STACK_YAML", yamlPath) : currentEnv)

silentRun :: (MonadIO m) => FilePath -> Text -> IO (Async a) -> m (Bool, Text)
silentRun yamlPath cmd spinnerM = do
  targetEnv <- provideYamlPath yamlPath
  let pc = setEnv targetEnv $ setStdout createPipe $ setStderr createPipe $ shell (toString cmd)
  liftIO
    $ withProcessWait pc
    $ \p -> do
      spinner <- spinnerM
      status <- waitExitCode p
      errCapture <- async $ do
        content <- TIO.hGetContents (getStderr p)
        evaluate (force content)
      cancel spinner
      rawLogsText <- wait errCapture
      let logsText = T.pack rawLogsText
      case status of
        ExitSuccess -> do
          pure (True, logsText)
        _ -> do
          pure (False, logsText)

inheritRun :: (MonadIO m) => FilePath -> Text -> m ()
inheritRun yamlPath cmd = do
  targetEnv <- provideYamlPath yamlPath
  let processConfig = setEnv targetEnv $ proc "/bin/sh" ["-c", toString cmd]
  liftIO (runProcess_ processConfig)
