{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.ConfigT
  ( ConfigT (..),
    runConfigT,
    VersionMap,
    updateConfig,
    Env (..),
    unpackConfigT,
    askCache,
    askMatrix,
    askVersion,
    saveConfig,
    resolveResultUI,
  )
where

import Control.Monad.Error.Class
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HWM.Core.Common (Check (..))
import HWM.Core.Formatting (Format (..))
import HWM.Core.Has (Has (..))
import HWM.Core.Options (Options (..))
import HWM.Core.Result (Issue (..), MonadIssue (..), Result (..), ResultT, runResultT)
import HWM.Core.Version (Version, askVersion)
import HWM.Domain.Config (Config (..))
import HWM.Domain.Environments (Environments (..))
import HWM.Domain.Workspace (PkgRegistry, WorkspaceGroup, pkgRegistry)
import HWM.Runtime.Cache (Cache, VersionMap, loadCache, saveCache)
import HWM.Runtime.Files (addHash, readYaml, rewrite_)
import HWM.Runtime.UI (MonadUI (..), UIT, printSummary, runUI)
import Relude

data Env (m :: Type -> Type) = Env
  { options :: Options,
    config :: Config,
    cache :: Cache,
    pkgs :: PkgRegistry
  }

type ConfigEnv = Env IO

newtype ConfigT (a :: Type) = ConfigT
  { _runConfigT :: ReaderT ConfigEnv (ResultT (UIT IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ConfigEnv,
      MonadError Issue,
      MonadIO
    )

instance Has (Env m) Options where
  obtain = options

instance Has (Env m) Cache where
  obtain = cache

instance Has (Env m) Config where
  obtain = config

instance Has (Env m) [WorkspaceGroup] where
  obtain Env {config} = workspace config

instance Has (Env m) Environments where
  obtain Env {config} = matrix config

instance Has (Env m) Version where
  obtain Env {config} = version config

instance Has (Env m) PkgRegistry where
  obtain = pkgs

instance MonadUI ConfigT where
  uiWrite txt = do
    Options {quiet} <- asks options
    unless quiet $ liftIO $ putStr (toString txt)
  uiIndentLevel = ConfigT $ lift uiIndentLevel
  uiWithIndent f (ConfigT (ReaderT action)) = ConfigT $ ReaderT (uiWithIndent f . action)

getFileHash :: FilePath -> IO (Maybe Text)
getFileHash filePath = do
  content <- T.decodeUtf8 <$> readFileBS filePath
  case T.lines content of
    (firstLine : _) ->
      case T.stripPrefix "# hash: " firstLine of
        Just hash -> pure (Just hash)
        Nothing -> pure Nothing
    [] -> pure Nothing

debug :: Text -> ConfigT ()
debug _ = pure ()

instance MonadIssue ConfigT where
  injectIssue = ConfigT . lift . injectIssue
  catchIssues (ConfigT action) = ConfigT $ ReaderT (catchIssues . runReaderT action)
  mapIssue f (ConfigT action) = ConfigT $ ReaderT $ \env -> mapIssue f (runReaderT action env)

computeHash :: Config -> Text
computeHash cfg =
  let hashInput = T.encodeUtf8 (T.pack (show (environments $ matrix cfg)))
      hashBytes = SHA256.hash hashInput
   in T.decodeUtf8 (Base16.encode hashBytes)

hasHashChanged :: Config -> Maybe Text -> Bool
hasHashChanged _ Nothing = True
hasHashChanged cfg (Just storedHash) = storedHash /= computeHash cfg

checkConfig :: ConfigT ()
checkConfig = do
  cfg <- asks config
  ops <- asks options
  check cfg
  debug $ "save " <> format (hwm ops)
  saveConfig cfg ops

saveConfig :: (MonadError Issue m, MonadIO m) => Config -> Options -> m ()
saveConfig config ops = do
  let file = hwm ops
  rewrite_ file (const $ pure config)
  addHash file (computeHash config)

updateConfig :: (Config -> ConfigT Config) -> ConfigT b -> ConfigT b
updateConfig f m = do
  config' <- asks config >>= f
  local (\e -> e {config = config'}) (checkConfig >> m)

runConfigT :: ConfigT () -> Options -> IO ()
runConfigT m opts@Options {..} = do
  config <- resolveResultTSilent (readYaml hwm)
  cache <- loadCache (defaultEnvironment (matrix config))
  changed <- hasHashChanged config <$> getFileHash hwm
  pkgs <- resolveResultTSilent (pkgRegistry (workspace config))
  let env = Env {options = opts, config, cache, pkgs}
      resultT = unpackConfigT (if changed then checkConfig >> m else m) env
  resolveResultT resultT cache

resolveResultT :: ResultT (UIT IO) a -> Cache -> IO ()
resolveResultT resT cache =
  runUI
    ( runResultT resT >>= \case
        Success {..} -> do
          liftIO $ saveCache cache
          printSummary issues
        Failure {..} -> do
          printSummary (toList failure)
          exitFailure
    )

resolveResultTSilent :: ResultT IO a -> IO a
resolveResultTSilent m = do
  r <- runResultT m
  case r of
    Success {..} -> pure result
    Failure {..} -> runUI (printSummary (toList failure)) >> exitFailure

resolveResultUI :: ResultT (UIT IO) a -> UIT IO a
resolveResultUI m = do
  r <- runResultT m
  case r of
    Success {..} -> pure result
    Failure {..} -> printSummary (toList failure) >> exitFailure

unpackConfigT :: ConfigT a -> ConfigEnv -> ResultT (UIT IO) a
unpackConfigT (ConfigT action) = runReaderT action

askCache :: ConfigT Cache
askCache = asks cache

askMatrix :: ConfigT Environments
askMatrix = asks (matrix . config)
