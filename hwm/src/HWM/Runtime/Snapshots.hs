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
import qualified Data.ByteString.Lazy.Char8 as BL
import HWM.Core.Result (Issue)
import HWM.Runtime.Cache (http)
import Relude

data ValueWrapper = ValueWrapper
  { snapshots :: [[(Text, Text, Text)]],
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

decodeWithError :: (MonadError Issue m, FromJSON a) => BL.ByteString -> m a
decodeWithError bs =
  case eitherDecode bs of
    Right val -> pure val
    Left err -> throwError $ stackageError err

stackage :: (MonadError Issue m, MonadIO m, FromJSON b) => [Text] -> m b
stackage path = do
  body <- http "https://www.stackage.org" path
  decodeWithError body

-- | Fetches the list of available Stackage snapshots from the public API
fetchStackageSnapshots :: (MonadIO m, MonadError Issue m) => m [SnapshotInfo]
fetchStackageSnapshots = do
  stackage ["api/v1/snapshots"] >>= \ValueWrapper {..} -> pure $ map (\(name, title, age) -> SnapshotInfo name title age) $ concat snapshots

fetchLtsSuggestions :: (MonadIO m, MonadError Issue m) => m (Map Text Text)
fetchLtsSuggestions = stackage ["download/snapshots.json"]