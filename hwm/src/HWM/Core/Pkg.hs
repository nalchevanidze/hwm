{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Pkg
  ( Pkg (..),
    PkgName (..),
    makePkg,
    pkgFile,
    mkPkgDirPath,
    resolvePrefix,
    IsPkg (..),
    scanPkgs,
    PkgSource (..),
    cabalSource,
    hpackSource,
    getVersionIssues,
    PackageIO (..),
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON (..), ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import Data.Text (intercalate)
import Data.Traversable (for)
import Data.Yaml.Aeson (ToJSON)
import Distribution.Package (packageName, pkgVersion)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageName
import HWM.Core.Common (Name)
import HWM.Core.Formatting
import HWM.Core.Has (Has)
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Core.Version (Version, askVersion, fromCabalVersion, toCabalVersion)
import HWM.Runtime.Files (cleanRelativePath)
import Relude hiding (Undefined, intercalate)
import System.Directory (listDirectory)
import System.FilePath
import System.FilePath.Glob (glob)

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
    pkgDirPath :: FilePath,
    cabalFile :: FilePath,
    hpackFile :: Maybe FilePath
  }
  deriving (Show, Ord, Eq)

data PkgSource = PkgSource {pkgSourceName :: Name, pkgSourceFile :: FilePath}

cabalSource :: Pkg -> PkgSource
cabalSource pkg = PkgSource (pkgMemberId pkg) (cabalFile pkg)

hpackSource :: Pkg -> Maybe PkgSource
hpackSource pkg = PkgSource (pkgMemberId pkg) <$> hpackFile pkg

class IsPkg a where
  getPkgName :: a -> PkgName

  -- version
  getPkgVersion :: a -> Version
  setVersion :: Version -> a -> a

class PackageIO a m where
  rewrite :: (a -> m (Maybe a)) -> Pkg -> m Status

instance IsPkg GenericPackageDescription where
  getPkgName = PkgName . toText . unPackageName . packageName . package . packageDescription
  getPkgVersion = fromCabalVersion . pkgVersion . package . packageDescription
  setVersion version gpd =
    let pd = packageDescription gpd
        pid = package pd
        newPid = pid {pkgVersion = toCabalVersion version}
        newPd = pd {package = newPid}
     in gpd {packageDescription = newPd}

pkgFile :: Pkg -> FilePath -> FilePath
pkgFile Pkg {..} file = normalise $ joinPath [pkgDirPath, file]

mkPkgDirPath :: Maybe String -> Maybe Text -> Text -> FilePath
mkPkgDirPath root prefix memberName = resolvePath root (resolvePrefix prefix memberName)

pickCabalFile :: (MonadError Issue m) => [FilePath] -> m FilePath
pickCabalFile [] = throwError "No .cabal file found in directory"
pickCabalFile [cabalFile] = pure cabalFile
pickCabalFile _ = throwError "Multiple .cabal files found in directory"

makePkg :: (MonadIO m, MonadError Issue m) => Text -> Maybe FilePath -> Maybe Name -> Name -> m Pkg
makePkg pkgGroup root prefix memberName = do
  let pkgDirPath = mkPkgDirPath root prefix memberName
  files <- liftIO $ listDirectory pkgDirPath
  cabal <- pickCabalFile [file | file <- files, takeExtension file == ".cabal"]
  let cabalFile = joinPath [pkgDirPath, cabal]
  let hpackFile = if "package.yaml" `elem` files then Just (joinPath [pkgDirPath, "package.yaml"]) else Nothing
  let pkgName = PkgName (toText $ dropExtension cabal)
  pure $ Pkg {pkgMemberId = if memberName == "." then "(root)" else memberName, ..}

scanPkgs :: (MonadIO m, MonadError Issue m) => FilePath -> m [Pkg]
scanPkgs root = do
  paths <- map (makeRelative root) <$> liftIO (glob "./**/**/*.cabal")
  for paths $ \path -> do
    let pkgDir = takeDirectory path
    let groupDir = cleanRelativePath (Just (takeDirectory pkgDir))
    let memberName = toText $ takeFileName pkgDir
    makePkg (maybe "" toText groupDir) groupDir Nothing memberName

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

getVersionIssues :: (MonadReader env m, Has env Version, IsPkg p) => PkgSource -> p -> m [Issue]
getVersionIssues source pkg = do
  expectedVersion <- askVersion
  let version = getPkgVersion pkg
  if version == expectedVersion
    then pure []
    else
      pure
        [ Issue
            { issueTopic = pkgSourceName source,
              issueMessage = "version mismatch: " <> format version <> " → " <> format expectedVersion,
              issueSeverity = SeverityWarning,
              issueDetails = Just GenericIssue {issueFile = pkgSourceFile source}
            }
        ]
