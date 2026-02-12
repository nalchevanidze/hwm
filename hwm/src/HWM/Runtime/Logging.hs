{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Logging
  ( logRoot,
    logPath,
    log,
    logError,
  )
where

import Data.Time (getCurrentTime)
import HWM.Core.Common (Name)
import Relude
import qualified System.IO as TIO

logRoot :: FilePath
logRoot = ".hwm/logs"

logPath :: Name -> FilePath
logPath name = logRoot <> "/" <> toString name <> ".log"

log :: (MonadIO m) => Name -> [(Text, Text)] -> Text -> m FilePath
log name table content = do
  timestamp <- liftIO getCurrentTime
  let logInfo = [("TIMESTAMP", show timestamp)]
  let path = logPath name
  let boxTop = "┌──────────────────────────────────────────────────────────"
      boxBottom = "└──────────────────────────────────────────────────────────"
      rows = map (\(k, v) -> "│ " <> k <> ": " <> v) (table <> logInfo)
      header = unlines (boxTop : rows <> [boxBottom, "", content, ""])
  liftIO $ TIO.appendFile path (toString header)
  pure path

logError :: (MonadIO m) => Name -> [(Text, Text)] -> Text -> m FilePath
logError name table = log name (table <> [("TYPE", "ERROR")])
