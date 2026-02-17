{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Cache
  ( Cache,
    Registry (..),
    askCache,
    getRegistry,
    updateRegistry,
    modifyCache,
    loadCache,
    saveCache,
    getVersions,
    Versions,
    VersionMap,
    clearVersions,
    prepareDir,
    getSnapshotGHC,
    Snapshot (..),
    getSnapshot,
    getVersion,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (withObject)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Yaml (Object, Parser, decodeEither', prettyPrintParseException)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Has (Has, askEnv)
import HWM.Core.Parsing (genUrl, parse, parsePkgString)
import HWM.Core.Pkg (PkgName (..))
import HWM.Core.Result (Issue, Result (..), ResultT (..))
import HWM.Core.Version (Version, parseGHCVersion)
import HWM.Runtime.Files (select)
import Network.HTTP.Req (GET (..), LbsResponse, NoReqBody (..), Option, Req, Url, defaultHttpConfig, lbsResponse, req, responseBody, runReq, useURI)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.URI (mkURI)

askCache :: (MonadReader env m, Has env Cache) => m Cache
askCache = askEnv

data Registry = Registry
  { currentEnv :: Name,
    versions :: Map PkgName Versions
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype Cache = Cache (STM.TVar Registry)

type Versions = NonEmpty Version

type VersionMap = Map PkgName Version

cacheDir :: FilePath
cacheDir = ".hwm/cache"

path :: FilePath
path = cacheDir <> "/state.json"

initRegistry :: Name -> Registry
initRegistry t = Registry {currentEnv = t, versions = mempty}

loadCache :: Name -> IO Cache
loadCache t = do
  exists <- doesFileExist path
  vm <-
    if exists
      then do
        bs <- BL.readFile path
        case Aeson.decode bs of
          Just vm' -> pure vm'
          Nothing -> pure (initRegistry t)
      else pure (initRegistry t)
  Cache <$> STM.newTVarIO vm

readCache :: Cache -> IO Registry
readCache (Cache tvar) = STM.readTVarIO tvar

modifyCache :: (MonadIO m) => Cache -> (Registry -> Registry) -> m ()
modifyCache (Cache tvar) f = liftIO $ STM.atomically $ STM.modifyTVar' tvar f

saveCache :: Cache -> IO ()
saveCache cache = do
  vm <- readCache cache
  createDirectoryIfMissing True cacheDir
  BL.writeFile path (Aeson.encode vm)

getRegistry :: (MonadReader env m, Has env Cache, MonadIO m) => m Registry
getRegistry = do
  Cache tvar <- askCache
  liftIO $ STM.readTVarIO tvar

updateRegistry :: (MonadReader env m, Has env Cache, MonadIO m) => (Registry -> Registry) -> m ()
updateRegistry f = do
  c <- askCache
  modifyCache c f

clearVersions :: (MonadReader env m, Has env Cache, MonadIO m) => m ()
clearVersions = updateRegistry (\reg -> reg {versions = mempty})

getReq :: (Url s, Option s) -> Req LbsResponse
getReq (u, o) = req GET u NoReqBody lbsResponse o

parseBody :: (MonadError Issue m) => Text -> m (Req LbsResponse)
parseBody url = do
  uri <- maybe (throwError $ fromString $ "Invalid Endpoint: " <> toString url <> "!") pure (mkURI url >>= useURI)
  pure (either getReq getReq uri)

http :: (MonadError Issue m, MonadIO m) => Text -> [Text] -> m BL.ByteString
http dom p = do
  request <- parseBody (genUrl dom p)
  responseBody <$> liftIO (runReq defaultHttpConfig request)

hackage :: (MonadIO m, MonadError Issue m) => Text -> m (Map Name (NonEmpty Version))
hackage name = http "https://hackage.haskell.org/package" [format name, "preferred.json"] >>= either (throwError . fromString) pure . eitherDecode

getVersions :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache) => PkgName -> m Versions
getVersions name = do
  Cache tvar <- askCache
  r <- liftIO $ STM.readTVarIO tvar
  case Map.lookup name (versions r) of
    Just vs -> pure vs
    Nothing -> do
      vs <- hackage (format name) >>= select "Field" "normal-version"
      modifyCache (Cache tvar) (\reg -> reg {versions = Map.singleton name vs <> versions reg})
      pure vs

prepareDir :: (MonadIO m) => FilePath -> m ()
prepareDir dir = liftIO $ createDirectoryIfMissing True dir

data Snapshot = Snapshot {snapshotCompiler :: Version, snapshotPackages :: Map PkgName Version}
  deriving (Show)

parseValue :: Value -> Parser (PkgName, Version)
parseValue = withObject "Package" $ \v -> do
  package <- v .: "hackage"
  let (name, versionStr) = parsePkgString package
  version <- parse versionStr
  pure (PkgName name, version)

parseMap :: [Value] -> Parser (Map PkgName Version)
parseMap pairs = Map.fromList <$> mapM parseValue pairs

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \v ->
    Snapshot <$> readCompilerVersion v <*> (v .: "packages" >>= parseMap)

readCompilerVersion :: Object -> Parser Version
readCompilerVersion v = do
  x <- (v .: "resolver" >>= (.: "compiler")) <|> v .: "compiler"
  parseGHCVersion x

genName :: (MonadError Issue m) => Text -> m [Text]
genName resolver
  | Just ltsNum <- T.stripPrefix "lts-" resolver = buildSegments "lts" "." ltsNum
  | Just nightlyDate <- T.stripPrefix "nightly-" resolver = buildSegments "nightly" "-" nightlyDate
  | otherwise = throwError $ fromString $ "Unsupported resolver: " <> toString resolver
  where
    buildSegments prefix delimiter value =
      case NE.nonEmpty (T.splitOn delimiter value) of
        Nothing -> throwError $ fromString $ "Malformed resolver: " <> toString resolver
        Just parts ->
          let segments = NE.init parts
              lastPart = NE.last parts
           in pure (prefix : segments <> [lastPart <> ".yaml"])

getSnapshotGHC :: (MonadIO m, MonadError Issue m) => Name -> m Version
getSnapshotGHC name = snapshotCompiler <$> getSnapshot name

getSnapshot :: (MonadError Issue m, MonadIO m) => Text -> m Snapshot
getSnapshot name = do
  pathSegments <- genName name
  body <- runResultT (http "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master" pathSegments)
  case body of
    Failure {failure} -> throwError $ fromString $ "HTTP Error: " <> show failure
    Success {result} -> case decodeEither' (BL.toStrict result) of
      Left err -> throwError $ fromString $ "Snapshot Error: " <> toString (T.intercalate "/" pathSegments) <> " - " <> prettyPrintParseException err
      Right snapshot -> pure snapshot

getVersion :: PkgName -> Snapshot -> Maybe Version
getVersion name snapshot = Map.lookup name (snapshotPackages snapshot)
