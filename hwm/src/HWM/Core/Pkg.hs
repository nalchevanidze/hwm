{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Pkg
  ( Pkg (..),
    PkgName (..),
    makePkg,
    pkgFile,
    cabalFilePath,
    mkPkgDirPath,
    resolvePrefix,
    IsPkg (..),
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON (..), ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import Data.Text (intercalate)
import Data.Yaml.Aeson (ToJSON)
import HWM.Core.Common (Name)
import HWM.Core.Formatting
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Runtime.Files (cleanRelativePath)
import Relude hiding (Undefined, intercalate)
import System.Directory (listDirectory)
import System.FilePath

-- |
--  Represents a reference to a package within the workspace.
--
--  This type serves as a lightweight pointer to a package, without exposing or depending on its internal structure or contents.
--  It is primarily used for identifying and referencing packages in workspace operations, enabling decoupled package management.
-- Package pointer without any insiight into content, used for referencing packages in the workspace
data Pkg = Pkg
  { pkgName :: PkgName,
    pkgGroup :: Name,
    pkgMemberId :: Name,
    pkgDirPath :: FilePath
  }
  deriving (Show, Ord, Eq)

class IsPkg a where
  getPkgName :: a -> PkgName
  getPkgVersion :: a -> Version


pkgFile :: Pkg -> FilePath -> FilePath
pkgFile Pkg {..} file = normalise $ joinPath [pkgDirPath, file]

cabalFilePath :: Pkg -> FilePath
cabalFilePath Pkg {..} = normalise $ joinPath [pkgDirPath, toString pkgName <> ".cabal"]

mkPkgDirPath :: Maybe String -> Maybe Text -> Text -> FilePath
mkPkgDirPath root prefix memberName = resolvePath root (resolvePrefix prefix memberName)

findCabalFile :: (MonadIO m) => FilePath -> m PkgName
findCabalFile dir = liftIO $ do
  contents <- listDirectory dir
  let ls = [name | name <- contents, takeExtension name == ".cabal"]
  case ls of
    [cabalFile] -> pure $ PkgName (toText $ dropExtension cabalFile)
    [] -> fail $ "No .cabal file found in directory: " <> dir
    _ -> fail $ "Multiple .cabal files found in directory: " <> dir

makePkg :: (MonadIO m, MonadError Issue m) => Text -> Maybe FilePath -> Maybe Name -> Name -> m Pkg
makePkg pkgGroup root prefix memberName = do
  let pkgDirPath = mkPkgDirPath root prefix memberName
  pkgName <- findCabalFile pkgDirPath
  pure $ Pkg {pkgMemberId = if memberName == "." then "(root)" else memberName, ..}

resolvePrefix :: Maybe Text -> Text -> Text
resolvePrefix prefix name = intercalate "-" (maybeToList prefix <> [name | name /= "."])

resolvePath :: (ToString a) => Maybe String -> a -> FilePath
resolvePath root path = normalise (joinPath (maybeToList (cleanRelativePath root) <> [toString path]))

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
