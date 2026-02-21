{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Archive
  ( createArchive,
    ArchivingPlan (..),
    ArchiveInfo (..),
    ArchiveFormat (..),
  )
where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.Release (ArchiveFormat (..), formatArchiveTemplate)
import HWM.Runtime.Platform (detectPlatform, platformExt)
import HWM.Runtime.Process (exec)
import Relude
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath, normalise, takeDirectory, takeFileName, (</>))

data ArchiveInfo = ArchiveInfo
  { archivePath :: FilePath,
    sha256Path :: FilePath
  }

data ArchivingPlan = ArchivingPlan
  { sourceDir :: FilePath,
    name :: Name,
    outDir :: FilePath,
    nameTemplate :: Text,
    archiveFormats :: [ArchiveFormat]
  }

createArchive ::
  (MonadIO m, MonadError Issue m, MonadIssue m) =>
  Version ->
  ArchivingPlan ->
  m [ArchiveInfo]
createArchive version ArchivingPlan {..} = do
  let binPath = sourceDir </> toString name
  binExists <- liftIO $ doesFileExist binPath
  unless binExists $ throwError (fromString $ "Binary not found at expected path: " <> binPath)
  platform <- detectPlatform

  fmap catMaybes $ forM archiveFormats $ \target -> do
    let ext = if target == Zip then ".zip" else ".tar.gz"
    let archiveName = formatArchiveTemplate name version platform nameTemplate <> ext
    let archivePath = normalise $ joinPath [outDir, toString archiveName]

    success <- writeArchive target binPath archivePath (name <> platformExt platform)

    if success
      then Just <$> finalizeArchive archivePath
      else pure Nothing

writeArchive :: (MonadIO m, MonadIssue m) => ArchiveFormat -> FilePath -> FilePath -> Name -> m Bool
writeArchive Zip binPath outPath binNameWithExt = liftIO $ do
  entry <- Zip.readEntry [] binPath
  let rootEntry = entry {Zip.eRelativePath = toString binNameWithExt}
  let archive = Zip.addEntryToArchive rootEntry Zip.emptyArchive
  BSL.writeFile outPath (Zip.fromArchive archive)
  pure True
writeArchive TarGz binPath outPath _ = do
  (isOk, _) <- exec "tar" ["-czvf", format outPath, "-C", format (takeDirectory binPath), format (takeFileName binPath)]
  unless isOk $ injectIssue (Issue {issueTopic = "archive", issueMessage = "⚠️  Warning: Failed to create .tar.gz (check if 'tar' is installed)", issueSeverity = SeverityWarning, issueDetails = Nothing})
  pure isOk

finalizeArchive :: (MonadIO m) => FilePath -> m ArchiveInfo
finalizeArchive archPath = do
  hashBS <- liftIO $ BS.readFile archPath
  let sha256 = T.decodeUtf8 (Base16.encode (SHA256.hash hashBS))
  let sha256Path = archPath <> ".sha256"
  liftIO $ T.writeFile sha256Path (format sha256)
  pure ArchiveInfo {archivePath = archPath, sha256Path = sha256Path}