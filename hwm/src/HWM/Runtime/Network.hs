{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Network (uploadToGitHub) where

import Control.Monad.Except (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Result (Issue (..))
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

-- | Specialized upload for GitHub to avoid the "Multipart" error
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
