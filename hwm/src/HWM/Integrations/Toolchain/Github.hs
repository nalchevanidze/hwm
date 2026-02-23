{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Github (ensureIsLatestTag) where

import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Formatting (Format (format))
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import Relude
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

ensureIsLatestTag :: (MonadIO m, MonadError Issue m) => Version -> m Text
ensureIsLatestTag targetTag = do
  (exitCode, out, _err) <- liftIO $ readProcessWithExitCode "git" ["describe", "--tags", "--abbrev=0"] ""
  case exitCode of
    ExitSuccess -> do
      let latestTag = T.strip (T.pack out)
      if latestTag == format targetTag || latestTag == "v" <> format targetTag
        then pure latestTag
        else throwError $ fromString $ toString ("Safety abort! You are trying to upload to an older release '" <> format targetTag <> "', but your latest tag is actually '" <> latestTag <> "'.")
    ExitFailure _ -> throwError $ fromString "Safety abort! Could not find any tags in the current Git history."
