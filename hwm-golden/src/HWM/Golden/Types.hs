{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Types
  ( ExpectedFiles (..),
    ChangeReport (..),
    CaseExpect (..),
    CaseRunner (..),
    CaseFile (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types ((.!=))
import qualified Data.Map.Strict as Map
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

data CaseExpect = CaseExpect
  { caseFailure :: Bool,
    caseFiles :: Maybe ExpectedFiles,
    caseCalls :: Maybe Value
  }

instance FromJSON CaseExpect where
  parseJSON = withObject "CaseExpect" $ \o -> do
    failure <- o .:? "failure"
    files <- o .:? "files"
    calls <- o .:? "calls"
    pure CaseExpect {caseFailure = fromMaybe False failure, caseFiles = files, caseCalls = calls}

instance ToJSON CaseExpect where
  toJSON CaseExpect {..} =
    dropEmpty
      $ object
        [ "failure" .= (if caseFailure then Just True else Nothing :: Maybe Bool),
          "files" .= caseFiles,
          "calls" .= caseCalls
        ]

data CaseRunner = CaseRunner
  { runnerEnv :: Maybe (Map.Map String String),
    runnerPath :: Maybe [FilePath],
    runnerBin :: Maybe (Map.Map String FilePath)
  }

instance FromJSON CaseRunner where
  parseJSON = withObject "CaseRunner" $ \o ->
    CaseRunner
      <$> o .:? "env"
      <*> o .:? "path"
      <*> o .:? "bin"

instance ToJSON CaseRunner where
  toJSON CaseRunner {..} =
    dropEmpty
      $ object
        [ "env" .= runnerEnv,
          "path" .= runnerPath,
          "bin" .= runnerBin
        ]

data CaseFile = CaseFile
  { caseProject :: FilePath,
    caseCommand :: String,
    caseRunner :: Maybe CaseRunner,
    caseExpect :: Maybe CaseExpect,
    caseName :: Maybe Text,
    caseNotes :: Maybe Text
  }

instance FromJSON CaseFile where
  parseJSON = withObject "CaseFile" $ \o ->
    CaseFile
      <$> o .: "project"
      <*> o .: "command"
      <*> o .:? "runner"
      <*> o .:? "expect"
      <*> o .:? "name"
      <*> o .:? "notes"

instance ToJSON CaseFile where
  toJSON CaseFile {..} =
    dropEmpty
      $ object
        [ "project" .= caseProject,
          "command" .= caseCommand,
          "runner" .= caseRunner,
          "name" .= caseName,
          "notes" .= caseNotes,
          "expect" .= caseExpect
        ]
