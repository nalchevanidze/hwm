{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release.Upload (publishToHackage) where

import Control.Exception (try)
import Control.Retry (exponentialBackoff, limitRetries)
import qualified Data.Text as T
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue (..), Severity (..))
import HWM.Domain.ConfigT (ConfigT)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Response (responseStatus))
import Network.HTTP.Client.MultipartFormData (partFileSource)
import Network.HTTP.Req
import Network.HTTP.Types (Status (..))
import Relude
import System.Timeout (timeout)
import UnliftIO.Retry (capDelay)

hwmConfig :: HttpConfig
hwmConfig =
  let policy =
        capDelay (5 * oneSecond) (exponentialBackoff oneSecond)
          <> limitRetries 3
   in defaultHttpConfig {httpConfigRetryPolicy = policy}

oneSecond :: Int
oneSecond = 1000000

publishToHackage :: Pkg -> FilePath -> ConfigT [Issue]
publishToHackage pkg tarballPath = do
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