{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Types
  ( ExpectedFiles (..),
    ChangeReport (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:?), (.=))
import Data.Aeson.Types ((.!=))
import HWM.Golden.Json (dropEmpty)
import Relude

data ExpectedFiles = ExpectedFiles
  { added :: [FilePath],
    deleted :: [FilePath],
    modified :: [FilePath],
    touched :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ExpectedFiles where
  toJSON ExpectedFiles {..} =
    dropEmpty
      $ object
        [ "added" .= added,
          "deleted" .= deleted,
          "modified" .= modified,
          "touched" .= touched
        ]

instance FromJSON ExpectedFiles where
  parseJSON = withObject "ExpectedFiles" $ \o ->
    ExpectedFiles
      <$> o .:? "added" .!= []
      <*> o .:? "deleted" .!= []
      <*> o .:? "modified" .!= []
      <*> o .:? "touched" .!= []

data ChangeReport = ChangeReport
  { files :: ExpectedFiles,
    calls :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChangeReport where
  toJSON ChangeReport {files = ExpectedFiles {..}, calls} =
    dropEmpty
      $ object
        [ "added" .= added,
          "deleted" .= deleted,
          "modified" .= modified,
          "touched" .= touched,
          "calls" .= calls
        ]

instance FromJSON ChangeReport where
  parseJSON = withObject "ChangeReport" $ \o ->
    ChangeReport
      <$> (ExpectedFiles <$> o .:? "added" .!= [] <*> o .:? "deleted" .!= [] <*> o .:? "modified" .!= [] <*> o .:? "touched" .!= [])
      <*> o .:? "calls"
