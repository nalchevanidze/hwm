{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Archive (createZipArchive, ArchiveInfo (..)) where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Result (Issue)
import HWM.Runtime.Platform (detectPlatform, platformExt)
import Relude
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath, normalise, (</>))

data ArchiveInfo = ArchiveInfo {zipPath :: FilePath, binName :: Name, sha256 :: T.Text}

createZipArchive ::
  (MonadIO m, MonadError Issue m) =>
  FilePath ->
  Name ->
  FilePath ->
  m ArchiveInfo
createZipArchive sourceDir name outDIr = do
  let binPath = sourceDir </> toString name
  binExists <- liftIO $ doesFileExist binPath
  unless binExists $ throwError (fromString $ "Binary not found at expected path: " <> binPath)

  platform <- detectPlatform
  let binName = name <> platformExt platform -- e.g., "morpheus.exe"
  let zipPath = normalise $ joinPath [outDIr, toString (name <> "-" <> format platform <> ".zip")]

  -- Read the binary from the disk into a Zip Entry
  entry <- liftIO $ Zip.readEntry [] binPath

  -- Rename the internal path so it sits at the root of the .zip file
  let rootEntry = entry {Zip.eRelativePath = toString binName}

  -- Add the entry to an empty archive and write to disk
  let archive = Zip.addEntryToArchive rootEntry Zip.emptyArchive
  liftIO $ BSL.writeFile zipPath (Zip.fromArchive archive)

  -- Compute SHA256 hash of the archive
  hashBS <- liftIO $ BS.readFile zipPath
  let sha256 = T.decodeUtf8 (Base16.encode (SHA256.hash hashBS))

  pure ArchiveInfo {..}