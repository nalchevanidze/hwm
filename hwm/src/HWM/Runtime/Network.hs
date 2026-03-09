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

ghAuth :: Text -> Option 'Https
ghAuth token =
  header "Authorization" ("Bearer " <> encodeUtf8 token)
    <> header "User-Agent" "hwm-tool"

hackageAuth :: Text -> Option 'Https
hackageAuth token =
  header "X-ApiKey" (encodeUtf8 token)
    <> header "User-Agent" "hwm-tool"

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
                <> ghAuth token
                <> header "Content-Type" "application/octet-stream" -- Crucial!
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

getGHUploadUrl :: (MonadIO m, MonadError Issue m) => Config -> Text -> m Text
getGHUploadUrl Config {..} tag = do
  gh <- maybe (throwError "GitHub repository not configured") pure cfgGithub
  token <- getGitHubToken
  liftIO $ runReq defaultHttpConfig $ do
    uri <- liftIO $ mkURI ("https://api.github.com/repos/" <> gh <> "/releases/tags/" <> tag)
    case useHttpsURI uri of
      Just (url, opts) -> do
        r <- req GET url NoReqBody jsonResponse (opts <> ghAuth token <> header "Accept" "application/vnd.github+json")
        let rawUrl = upload_url (responseBody r)
        -- Strip the "{?name,label}" template suffix before returning
        return $ T.takeWhile (/= '{') rawUrl
      Nothing -> error "GitHub API URLs must be HTTPS"

mkSecond :: Int -> Int
mkSecond n = n * 1000000

hwmConfig :: HttpConfig
hwmConfig =
  let policy =
        capDelay (mkSecond 5) (exponentialBackoff (mkSecond 1))
          <> limitRetries 3
   in defaultHttpConfig {httpConfigRetryPolicy = policy}

safeReq :: (MonadIO m) => Pkg -> Text -> (Status -> Issue) -> IO b -> m (Either Issue b)
safeReq pkg errMsg f action = do
  res <- liftIO $ timeout (mkSecond 90) $ try action
  case res of
    Nothing -> pure $ Left $ Issue (pkgMemberId pkg) SeverityError (errMsg <> " (Timeout)") Nothing
    Just (Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException r _)))) -> pure $ Left $ f (responseStatus r)
    Just (Left e) -> pure $ Left $ networkError pkg e
    Just (Right a) -> pure $ Right a

getHackageToken :: (MonadIO m, MonadError Issue m) => m Text
getHackageToken = do
  maybeToken <- liftIO $ lookupEnv "HACKAGE_AUTH_TOKEN"
  maybe
    (throwError "HACKAGE_AUTH_TOKEN environment variable not set. Please set it to a valid Hackage API Token.")
    (pure . T.pack)
    maybeToken

uploadToHackage :: Pkg -> FilePath -> ConfigT [Issue]
uploadToHackage pkg tarballPath = do
  token <- getHackageToken
  let auth = hackageAuth token
  let url = https "hackage.haskell.org" /: "packages"
  body <- reqBodyMultipart [partFileSource "package" tarballPath]
  result <-
    safeReq pkg "Hackage Upload Failed" (handleHttpError pkg)
      $ runReq hwmConfig
      $ req POST url body ignoreResponse auth
  pure $ either pure (const []) result

handleHttpError :: Pkg -> Status -> Issue
handleHttpError pkg status = case statusCode status of
  409 -> warn pkg "Version already exists on Hackage. Skipping."
  401 -> err pkg "Invalid Hackage API Token."
  413 -> err pkg "Package tarball is too large for Hackage."
  _ -> err pkg $ "Hackage returned: " <> show (statusCode status)

networkError :: (Show a) => Pkg -> a -> Issue
networkError pkg e = Issue (pkgMemberId pkg) SeverityError ("[Hackage Connection Error]: " <> show e) Nothing

err :: Pkg -> Text -> Issue
err pkg msg = Issue (pkgMemberId pkg) SeverityError msg Nothing

warn :: Pkg -> Text -> Issue
warn pkg msg = Issue (pkgMemberId pkg) SeverityWarning msg Nothing
