{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Logging (logIssue, logCommandStart, logCommandEnd, debug, logRoot, logPath) where

import Data.Time (getCurrentTime)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (Cyan), chalk)
import HWM.Core.Options (whenCI, whenDebug)
import HWM.Core.Result (Severity)
import HWM.Runtime.Files (prepareDir)
import HWM.Runtime.UI (MonadUI, putLine)
import Relude
import qualified System.IO as TIO
import System.Process.Typed (ExitCode (..))

logRoot :: FilePath
logRoot = ".hwm/logs"

logPath :: Name -> FilePath
logPath name = logRoot <> "/" <> toString name <> ".log"

logIssue :: (MonadIO m) => Name -> Severity -> [(Text, Text)] -> Text -> m FilePath
logIssue name severity table content = do
  prepareDir logRoot
  timestamp <- liftIO getCurrentTime
  let logInfo = [("TIMESTAMP", show timestamp), ("SEVERITY", show severity)]
  let path = logPath name
  liftIO $ TIO.withFile path TIO.AppendMode $ \logHandle -> writeLogEntry logHandle (table <> logInfo) content
  whenCI $ putTextLn content
  pure path

logCommandStart :: TIO.Handle -> Text -> IO ()
logCommandStart logHandle cmd =
  writeLogEntry logHandle [("EVENT", "COMMAND_START"), ("COMMAND", cmd)] "Command output follows."

logCommandEnd :: TIO.Handle -> ExitCode -> IO ()
logCommandEnd logHandle code =
  writeLogEntry logHandle [("EVENT", "COMMAND_END"), ("EXIT_CODE", exitCodeSummary code)] "Command finished."

debug :: (MonadIO m, MonadUI m) => Text -> m ()
debug msg = whenDebug $ putLine $ chalk Cyan "[DEBUG] " <> msg

writeLogEntry :: TIO.Handle -> [(Text, Text)] -> Text -> IO ()
writeLogEntry logHandle table content = do
  TIO.hPutStr logHandle (toString (formatLogEntry table content))
  TIO.hFlush logHandle

formatLogEntry :: [(Text, Text)] -> Text -> Text
formatLogEntry table content =
  let boxTop = "┌──────────────────────────────────────────────────────────"
      boxBottom = "└──────────────────────────────────────────────────────────"
      rows = map (\(k, v) -> "│ " <> k <> ": " <> v) table
   in unlines (boxTop : rows <> [boxBottom, "", content, ""])

exitCodeSummary :: ExitCode -> Text
exitCodeSummary ExitSuccess = "0"
exitCodeSummary (ExitFailure code) = show code
