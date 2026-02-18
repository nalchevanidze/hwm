{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Snapshots (fetchStackageSnapshots, fetchLtsSuggestions, SnapshotInfo (..)) where

import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Decoding (eitherDecode)
import HWM.Core.Result (Issue)
import HWM.Runtime.Cache (http)
import Relude

data WRapper = WRapper
  { snapshots :: [SnapshotInfo],
    totalCount :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data SnapshotInfo = SnapshotInfo
  { snapshotName :: Text,
    snapshotTitle :: Text,
    snapshotAge :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

stackageError :: String -> Issue
stackageError = fromString . ("Failed to decode Stackage API response: " <>)

-- | Fetches the list of available Stackage snapshots from the public API
fetchStackageSnapshots :: (MonadIO m, MonadError Issue m) => m [SnapshotInfo]
fetchStackageSnapshots = do
  body <- http "https://www.stackage.org/api/v1/" ["snapshots"]
  case eitherDecode body of
    Right WRapper {..} -> pure snapshots
    Left err -> throwError $ stackageError err

fetchLtsSuggestions :: (MonadIO m, MonadError Issue m) => m (Map Text Text)
fetchLtsSuggestions = do
  liftIO $ putStrLn "Fetching LTS suggestions from Stackage API..." -- Debugging output, can be removed later
  eBody <- http "https://www.stackage.org/download/" ["snapshots.json"]
  liftIO $ putStrLn "Delivered..." -- Debugging output, can be removed later
  case eitherDecode eBody of
    Right o -> pure o
    Left err -> throwError $ stackageError err