{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Logging (logRoot, logCommandStart, logCommandEnd, debug, logPath, rotateLogs, genLogId) where

import Data.List (isSuffixOf)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (Cyan), Format (..), chalk, slugifyList)
import HWM.Core.Options (whenDebug)
import HWM.Runtime.UI (MonadUI, putLine)
import Relude
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.IO as TIO
import System.Process.Typed (ExitCode (..))

logRoot :: FilePath
logRoot = ".hwm/logs"

logPath :: Name -> FilePath
logPath name = logRoot <> "/" <> toString name <> ".log"

logCommandStart :: TIO.Handle -> Text -> IO ()
logCommandStart logHandle cmd =
  writeLogEntry logHandle [("EVENT", "COMMAND_START"), ("COMMAND", cmd)]

logCommandEnd :: TIO.Handle -> ExitCode -> IO ()
logCommandEnd logHandle code =
  writeLogEntry logHandle [("EVENT", "COMMAND_END"), ("EXIT_CODE", exitCodeSummary code)]

debug :: (MonadIO m, MonadUI m) => Text -> m ()
debug msg = whenDebug $ putLine $ chalk Cyan "[DEBUG] " <> msg

writeLogEntry :: TIO.Handle -> [(Text, Text)] -> IO ()
writeLogEntry logHandle table = do
  timestamp <- liftIO getCurrentTime
  TIO.hPutStr logHandle (toString (formatLogEntry (table <> [("TIMESTAMP", show timestamp)])))
  TIO.hFlush logHandle

formatLogEntry :: [(Text, Text)] -> Text
formatLogEntry table =
  let boxTop = "┌──────────────────────────────────────────────────────────"
      boxBottom = "└──────────────────────────────────────────────────────────"
      rows = map (\(k, v) -> "│ " <> k <> ": " <> v) table
   in unlines (boxTop : rows <> [boxBottom, ""])

exitCodeSummary :: ExitCode -> Text
exitCodeSummary ExitSuccess = "0"
exitCodeSummary (ExitFailure code) = show code

-- 1. Generate the Timestamp once per CLI run
genLogId :: (MonadIO m) => Text -> m Text
genLogId prefix = liftIO $ do
  fixed <- Env.lookupEnv "HWM_LOG_ID_FIXED"
  case fixed of
    Just seed -> pure $ slugifyList [toText seed, prefix]
    Nothing -> do
      timestamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
      pure $ slugifyList [format timestamp, prefix]

-- 2. The Cleanup Function
rotateLogs :: (MonadIO m) => m ()
rotateLogs = liftIO $ do
  createDirectoryIfMissing True logRoot
  files <- listDirectory logRoot
  -- Only look at .log files, sort them alphabetically (oldest first)
  let logFiles = sort $ filter (\f -> ".log" `isSuffixOf` f) files

  -- If we have 8 files and want to keep 5, we drop the first 3 (the oldest ones)
  let filesToDelete = take (length logFiles - 12) logFiles

  forM_ filesToDelete $ \f ->
    removeFile (logRoot </> f)
