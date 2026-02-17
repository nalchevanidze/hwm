{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Pkg
  ( Pkg (..),
    PkgName (..),
    makePkg,
    pkgFile,
    pkgYamlPath,
    pkgId,
    scanPkgs,
    cabalFilePath,
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON (..), ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import qualified Data.Map as Map
import Data.Text (intercalate)
import Data.Traversable (for)
import Data.Yaml.Aeson (ToJSON)
import HWM.Core.Common (Name)
import HWM.Core.Formatting
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Runtime.Files (cleanRelativePath, readYaml)
import Relude hiding (Undefined, intercalate)
import System.FilePath (makeRelative, takeDirectory)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (joinPath, normalise, takeFileName, (</>))

data PkgInfo = PkgInfo {name :: PkgName, version :: Version}
  deriving (Generic, FromJSON, Show)

data Pkg = Pkg
  { pkgName :: PkgName,
    pkgVersion :: Version,
    pkgGroup :: Name,
    pkgMemberId :: Name,
    pkgDirPath :: FilePath
  }
  deriving (Show, Ord, Eq)

packageYamlFileName :: String
packageYamlFileName = "package.yaml"

-- Helper to ensure "package.yaml" is only appended if not already present
ensurePackageYaml :: FilePath -> FilePath
ensurePackageYaml path
  | takeFileName path == packageYamlFileName = path
  | otherwise = normalise $ path </> packageYamlFileName

pkgYamlPath :: Pkg -> FilePath
pkgYamlPath pkg = pkgFile pkg packageYamlFileName

getPkgInfo :: (MonadError Issue m, MonadIO m) => FilePath -> m PkgInfo
getPkgInfo = readYaml . normalise . ensurePackageYaml

pkgFile :: Pkg -> FilePath -> FilePath
pkgFile Pkg {..} file = normalise $ joinPath [pkgDirPath, file]

cabalFilePath :: Pkg -> FilePath
cabalFilePath Pkg {..} = normalise $ joinPath [pkgDirPath, toString pkgName <> ".cabal"]

pkgId :: Pkg -> Text
pkgId Pkg {pkgGroup, pkgMemberId} = pkgGroup <> "/" <> pkgMemberId

toPkg :: PkgInfo -> Name -> Name -> FilePath -> Pkg
toPkg PkgInfo {name, version} groupName memberName dir =
  Pkg
    { pkgName = name,
      pkgVersion = version,
      pkgGroup = groupName,
      pkgMemberId = if memberName == "." then "(root)" else memberName,
      pkgDirPath = dir
    }

makePkg :: (MonadIO m, MonadError Issue m) => Text -> Maybe FilePath -> Maybe Name -> Name -> m Pkg
makePkg groupName root prefix memberName = do
  let pkgDirPath = resolvePath root (resolvePrefix prefix memberName)
  json <- getPkgInfo pkgDirPath
  pure $ toPkg json groupName memberName pkgDirPath

resolvePrefix :: Maybe Text -> Text -> Text
resolvePrefix prefix name = intercalate "-" (maybeToList prefix <> [name | name /= "."])

resolvePath :: (ToString a) => Maybe String -> a -> FilePath
resolvePath root path = normalise (joinPath (maybeToList (cleanRelativePath root) <> [toString path]))

scanPkgInfos :: (MonadIO m, MonadError Issue m) => FilePath -> m (Map FilePath PkgInfo)
scanPkgInfos root = do
  paths <- map (makeRelative root) <$> liftIO (glob $ normalise "./**/**/package.yaml")
  pkgInfos <- traverse getPkgInfo paths
  pure $ Map.fromList (zip paths pkgInfos)

scanPkgs :: (MonadIO m, MonadError Issue m) => FilePath -> m [Pkg]
scanPkgs root = do
  infos <- scanPkgInfos root
  for (Map.toList infos) $ \(path, info) -> do
    let pkgDir = takeDirectory path
    let memberName = toText $ takeFileName pkgDir
    let groupName = maybe "" toText $ cleanRelativePath (Just (takeDirectory pkgDir))
    pure $ toPkg info groupName memberName pkgDir

newtype PkgName = PkgName Text
  deriving newtype
    ( FromJSON,
      ToJSON,
      Show,
      Ord,
      Eq,
      FromJSONKey,
      ToJSONKey,
      ToString
    )

instance Format PkgName where
  format (PkgName x) = x

instance Parse PkgName where
  parse = pure . PkgName
