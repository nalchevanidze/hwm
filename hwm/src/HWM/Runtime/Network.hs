{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Network (uploadToGitHub, getGHUploadUrl, uploadToHackage) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (..))
import Control.Retry (exponentialBackoff, limitRetries)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue (..), Severity (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Response (responseStatus))
import Network.HTTP.Client.MultipartFormData (partFileSource)
import Network.HTTP.Req
import Network.HTTP.Types (Status (..))
import Relude
import System.FilePath (takeFileName)
import System.Timeout (timeout)
import Text.URI (mkURI)
import UnliftIO.Retry (capDelay)

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

hwmConfig :: HttpConfig
hwmConfig =
  let policy =
        capDelay (5 * oneSecond) (exponentialBackoff oneSecond)
          <> limitRetries 3
   in defaultHttpConfig {httpConfigRetryPolicy = policy}

oneSecond :: Int
oneSecond = 1000000

uploadToHackage :: Pkg -> FilePath -> ConfigT [Issue]
uploadToHackage pkg tarballPath = do
  mToken <- liftIO $ lookupEnv "HACKAGE_AUTH_TOKEN"
  case mToken of
    Nothing -> pure [Issue (pkgMemberId pkg) SeverityError "HACKAGE_AUTH_TOKEN not set" Nothing]
    Just token -> do
      body <- reqBodyMultipart [partFileSource "package" tarballPath]

      let url = https "hackage.haskell.org" /: "packages"
      let auth = header "Authorization" ("X-ApiKey " <> encodeUtf8 (T.pack token))

      result <-
        liftIO
          $ liftIO
            ( timeout (90 * oneSecond)
                $ try
                $ runReq hwmConfig
                $ req POST url body ignoreResponse auth
            )
      case result of
        Just (Right _) -> pure []
        Nothing ->
          pure [Issue (pkgMemberId pkg) SeverityError "Global timeout: Upload took > 90s" Nothing]
        Just (Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException res _)))) ->
          handleHttpError pkg (responseStatus res)
        Just (Left e) ->
          pure [networkError pkg e]

-- | Handle specific Hackage API responses
handleHttpError :: Pkg -> Status -> ConfigT [Issue]
handleHttpError pkg status = case statusCode status of
  409 -> pure [warn pkg "Version already exists on Hackage. Skipping."]
  401 -> pure [err pkg "Invalid Hackage API Token."]
  413 -> pure [err pkg "Package tarball is too large for Hackage."]
  _ -> pure [err pkg $ "Hackage returned: " <> show (statusCode status)]

networkError :: (Show a) => Pkg -> a -> Issue
networkError pkg e = Issue (pkgMemberId pkg) SeverityError ("[Hackage Connection Error]: " <> show e) Nothing

err :: Pkg -> Text -> Issue
err pkg msg = Issue (pkgMemberId pkg) SeverityError msg Nothing

warn :: Pkg -> Text -> Issue
warn pkg msg = Issue (pkgMemberId pkg) SeverityWarning msg Nothing
