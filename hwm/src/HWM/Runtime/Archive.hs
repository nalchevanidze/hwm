{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Archive
  ( createArchive,
    ArchiveOptions (..),
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
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Release (ArchiveFormat (..), formatArchiveTemplate)
import HWM.Runtime.Platform (OS (..), Platform (..), detectPlatform, platformExt)
import Relude
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath, normalise, takeDirectory, takeFileName, (</>))
import System.Process (callProcess)

data ArchiveInfo = ArchiveInfo
  { archivePath :: FilePath,
    binName :: Name,
    sha256Path :: FilePath
  }

data ArchiveOptions = ArchiveOptions
  { sourceDir :: FilePath,
    name :: Name,
    outDir :: FilePath,
    nameTemplate :: Text,
    formatPreference :: ArchiveFormat
  }

data Target = TargetZip | TargetTarGz
  deriving (Show, Eq)

createArchive ::
  (MonadIO m, MonadError Issue m) =>
  Version ->
  ArchiveOptions ->
  m ArchiveInfo
createArchive version ArchiveOptions {..} = do
  let binPath = sourceDir </> toString name
  binExists <- liftIO $ doesFileExist binPath
  unless binExists $ throwError (fromString $ "Binary not found at expected path: " <> binPath)

  platform <- detectPlatform

  -- 1. Determine the actual format based on 'Auto' logic
  let actualFormat = case (formatPreference, os platform) of
        (Auto, Windows) -> TargetZip
        (Zip, _) -> TargetZip
        (Auto, _) -> TargetTarGz
        (TarGz, _) -> TargetTarGz

  let ext = if actualFormat == TargetZip then ".zip" else ".tar.gz"

  let archiveName = formatArchiveTemplate name version platform nameTemplate <> ext
  let binName = name <> platformExt platform
  let archivePath = normalise $ joinPath [outDir, toString archiveName]

  -- 2. Execute the archiving
  liftIO $ case actualFormat of
    TargetZip -> do
      entry <- Zip.readEntry [] binPath
      let rootEntry = entry {Zip.eRelativePath = toString binName}
      let archive = Zip.addEntryToArchive rootEntry Zip.emptyArchive
      BSL.writeFile archivePath (Zip.fromArchive archive)
    TargetTarGz -> do
      -- We use system tar to preserve +x permissions on Unix
      callProcess
        "tar"
        [ "-czvf",
          archivePath,
          "-C",
          takeDirectory binPath,
          takeFileName binPath
        ]

  -- 3. Compute SHA256 (Works for both formats)
  hashBS <- liftIO $ BS.readFile archivePath
  let sha256 = T.decodeUtf8 (Base16.encode (SHA256.hash hashBS))
  let sha256Path = archivePath <> ".sha256"
  liftIO $ T.writeFile sha256Path (format sha256)

  pure ArchiveInfo {..}