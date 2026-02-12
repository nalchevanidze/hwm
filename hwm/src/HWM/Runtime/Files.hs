{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Files
  ( readYaml,
    rewrite_,
    statusM,
    aesonYAMLOptions,
    select,
    remove,
    addHash,
    forbidOverride,
    cleanRelativePath,
  )
where

import Control.Exception (catch, throwIO, tryJust)
import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson
  ( FromJSON (..),
    Object,
    Options (..),
    ToJSON (..),
    Value (..),
    defaultOptions,
  )
import Data.ByteString (readFile, writeFile)
import Data.Char (isUpper, toLower)
import Data.List (elemIndex, stripPrefix)
import Data.Map (lookup)
import Data.Text (toTitle)
import qualified Data.Text.Encoding as T
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HWM.Core.Formatting
import HWM.Core.Result (Issue)
import Relude hiding (readFile, writeFile)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (joinPath, splitDirectories)
import System.IO.Error (isDoesNotExistError)

printException :: SomeException -> String
printException = show

safeIO :: IO a -> IO (Either String a)
safeIO = tryJust (Just . printException)

remove :: (MonadIO m) => FilePath -> m ()
remove file = liftIO $ removeFile file `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))

safeRead :: (MonadIO m) => FilePath -> m (Either String ByteString)
safeRead = liftIO . safeIO . readFile

safeWrite :: (MonadIO m) => FilePath -> ByteString -> m (Either String ())
safeWrite file content = liftIO $ safeIO (writeFile file content)

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml =
  encodePretty
    $ setConfDropNull True
    $ setConfCompare compareFields defConfig

data Yaml t = Yaml
  { getData :: t,
    rawValue :: Object
  }
  deriving (Generic)

instance (FromJSON t) => FromJSON (Yaml t) where
  parseJSON v = Yaml <$> parseJSON v <*> parseJSON v

instance (ToJSON t) => ToJSON (Yaml t) where
  toJSON (Yaml t v) = Object (toObject (toJSON t) <> v)

toObject :: Value -> Object
toObject (Object x) = x
toObject _ = mempty

mapYaml :: (Functor m) => (Maybe t -> m t) -> Maybe (Yaml t) -> m (Yaml t)
mapYaml f (Just (Yaml v props)) = (`Yaml` props) <$> f (Just v)
mapYaml f Nothing = (`Yaml` mempty) <$> f Nothing

fromEither :: (MonadError Issue m, FromJSON b, MonadIO m) => Either a ByteString -> m (Maybe b)
fromEither = either (const $ pure Nothing) (fmap Just . liftIO . decodeThrow)

withThrow :: (MonadError Issue m) => m (Either String a) -> m a
withThrow x = x >>= either (throwError . fromString) pure

rewrite_ :: (MonadError Issue m, MonadIO m, FromJSON t, ToJSON t) => FilePath -> (Maybe t -> m t) -> m ()
rewrite_ pkg f = do
  original <- safeRead pkg
  yaml <- fromEither original >>= mapYaml f
  withThrow (safeWrite pkg (serializeYaml yaml))

statusM :: (MonadIO m) => FilePath -> m t -> m Status
statusM pkg m = do
  before <- safeRead pkg
  _ <- m
  after <- safeRead pkg
  pure (if before == after then Checked else Updated)

readYaml :: (MonadError Issue m, MonadIO m, FromJSON a) => FilePath -> m a
readYaml = withThrow . safeRead >=> (liftIO . decodeThrow)

fields :: [Text]
fields =
  map
    toTitle
    [ "name",
      "version",
      "github",
      "license",
      "author",
      "category",
      "synopsis",
      "maintainer",
      "homepage",
      "copyright",
      "license-file",
      "description",
      "bounds",
      "ghc",
      "resolver",
      "packages",
      "workspace",
      "builds",
      "extra-source-files",
      "data-files",
      "main",
      "source-dirs",
      "ghc-options",
      "dependencies",
      "library",
      "executables",
      "include",
      "exclude",
      "allow-newer",
      "save-hackage-creds",
      "extra-deps",
      "stackYaml",
      "components",
      "path",
      "component"
    ]

toPriority :: Text -> Int
toPriority = fromMaybe (length fields) . (`elemIndex` fields)

mapTuple :: (a -> b) -> (b -> b -> c) -> a -> a -> c
mapTuple f g a b = g (f a) (f b)

compareFields :: Text -> Text -> Ordering
compareFields = mapTuple toTitle (mapTuple toPriority compare <> compare)

toKebabCase :: String -> String
toKebabCase = concatMap toKebab
  where
    toKebab x
      | isUpper x = ['-', toLower x]
      | otherwise = [x]

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase, omitNothingFields = True}

select :: (MonadError Issue m, Format t, Ord t) => Text -> t -> Map t a -> m a
select e k = maybe (throwError $ fromString $ "Unknown " <> toString e <> ": " <> toString (format k) <> "!") pure . lookup k

addHash :: (MonadIO m) => FilePath -> Text -> m ()
addHash filePath hash = do
  content <- liftIO $ T.decodeUtf8 <$> readFileBS filePath
  let contentWithHash = "# hash: " <> hash <> "\n" <> content
  liftIO $ writeFileBS filePath (T.encodeUtf8 contentWithHash)

forbidOverride :: (MonadIO m, MonadError e m, IsString e) => FilePath -> m ()
forbidOverride path = do
  exists <- liftIO $ doesFileExist path
  when exists $ throwError $ fromString $ "File \"" <> path <> "\" already exists!"

cleanRelativePath :: Maybe String -> Maybe String
cleanRelativePath Nothing = Nothing
cleanRelativePath (Just "") = Nothing
cleanRelativePath (Just "./") = Nothing
cleanRelativePath (Just ".") = Nothing
cleanRelativePath (Just name) =
  Just
    $ joinPath
    $ splitDirectories
    $ fromMaybe name (stripPrefix "./" name)
