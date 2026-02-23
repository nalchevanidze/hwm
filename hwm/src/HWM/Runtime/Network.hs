{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Network (uploadToGitHub, getGHUploadUrl) where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import HWM.Core.Result (Issue (..))
import HWM.Domain.Config (Config (..))
import Network.HTTP.Req
import Relude
import System.FilePath (takeFileName)
import Text.URI (mkURI)

getGitHubToken :: (MonadIO m, MonadError Issue m) => m Text
getGitHubToken = do
  maybeToken <- liftIO $ lookupEnv "GITHUB_TOKEN"
  maybe
    (throwError "GITHUB_TOKEN environment variable not set. Please set it to a valid GitHub Personal Access Token with repo permissions.")
    (pure . T.pack)
    maybeToken

uploadToGitHub :: (MonadIO m, MonadError Issue m) => Text -> FilePath -> m ()
uploadToGitHub uploadUrl filePath = do
  token <- getGitHubToken
  liftIO $ runReq defaultHttpConfig $ do
    uri <- liftIO $ mkURI uploadUrl
    case useHttpsURI uri of
      Just (url, opts) -> do
        let fileName = T.pack $ takeFileName filePath
        void
          $ req
            POST
            url
            (ReqBodyFile filePath) -- Raw bytes, no multipart wrapping
            ignoreResponse
            ( opts
                <> queryParam "name" (Just fileName) -- Required by GitHub
                <> header "Authorization" ("Bearer " <> encodeUtf8 token)
                <> header "Content-Type" "application/octet-stream" -- Crucial!
                <> header "User-Agent" "hwm-tool" -- GitHub requires a User-Agent
            )
      Nothing -> liftIO $ putStrLn "GitHub Upload URLs must be HTTPS"

-- 1. Define a tiny data type to represent the GitHub JSON response.
-- Aeson automatically maps the "upload_url" JSON key to this record field.

data GitHubRelease = GitHubRelease
  { name :: Text,
    upload_url :: Text
  }
  deriving (Show, Generic)

-- 2. Automatically derive the JSON parser
instance FromJSON GitHubRelease

-- 3. The main function returning the clean URL
getGHUploadUrl :: (MonadIO m, MonadError Issue m) => Config -> Text -> m Text
getGHUploadUrl Config {..} tag = do
  gh <- maybe (throwError "GitHub repository not configured") pure cfgGithub
  token <- getGitHubToken
  liftIO $ runReq defaultHttpConfig $ do
    -- Construct the endpoint URL
    let urlStr = "https://api.github.com/repos/" <> gh <> "/releases/tags/" <> tag
    uri <- liftIO $ mkURI urlStr
    case useHttpsURI uri of
      Just (url, opts) -> do
        -- Execute the GET request, expecting a JSON response matching our GitHubRelease type
        r <-
          req
            GET
            url
            NoReqBody
            jsonResponse -- This automatically parses the ByteString into our GitHubRelease data type!
            ( opts
                <> header "Authorization" ("Bearer " <> encodeUtf8 token)
                <> header "Accept" "application/vnd.github+json"
                <> header "User-Agent" "hwm-tool"
            )

        -- Extract the raw URL from the parsed JSON object
        let rawUrl = upload_url (responseBody r)

        -- Strip the "{?name,label}" template suffix before returning
        return $ T.takeWhile (/= '{') rawUrl
      Nothing -> error "GitHub API URLs must be HTTPS"
