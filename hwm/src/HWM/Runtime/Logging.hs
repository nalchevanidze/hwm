{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Logging (logIssue) where

import Data.Time (getCurrentTime)
import HWM.Core.Common (Name)
import HWM.Core.Options (whenCI)
import HWM.Core.Result (Severity)
import HWM.Runtime.Cache (prepareDir)
import HWM.Runtime.UI (MonadUI, putLine)
import Relude
import qualified System.IO as TIO

logRoot :: FilePath
logRoot = ".hwm/logs"

logPath :: Name -> FilePath
logPath name = logRoot <> "/" <> toString name <> ".log"

logIssue :: (MonadIO m, MonadUI m) => Name -> Severity -> [(Text, Text)] -> Text -> m FilePath
logIssue name severity table content = do
  prepareDir logRoot
  timestamp <- liftIO getCurrentTime
  let logInfo = [("TIMESTAMP", show timestamp), ("SEVERITY", show severity)]
  let path = logPath name
  let boxTop = "┌──────────────────────────────────────────────────────────"
      boxBottom = "└──────────────────────────────────────────────────────────"
      rows = map (\(k, v) -> "│ " <> k <> ": " <> v) (table <> logInfo)
      header = unlines (boxTop : rows <> [boxBottom, "", content, ""])
  liftIO $ TIO.appendFile path (toString header)
  whenCI $ putLine content
  pure path
