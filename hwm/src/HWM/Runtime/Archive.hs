{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Archive (createZipArchive) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BSL
import Relude

-- | Creates a zip archive containing a single executable at the root level.
-- Later, we will expand this to accept a list of 'include' files (like LICENSE).
createZipArchive :: 
     FilePath -- ^ The path to the source binary on disk (e.g., ".hwm/release/morpheus.exe")
  -> FilePath -- ^ The internal name it should have inside the zip (e.g., "morpheus.exe")
  -> FilePath -- ^ The final output path of the .zip file (e.g., "morpheus-v1-linux.zip")
  -> IO ()
createZipArchive sourcePath internalName outputPath = do
  -- Read the binary from the disk into a Zip Entry
  entry <- Zip.readEntry [Zip.OptDeflate] sourcePath
  
  -- Rename the internal path so it sits at the root of the .zip file
  let rootEntry = entry { Zip.eRelativePath = internalName }
  
  -- Add the entry to an empty archive and write to disk
  let archive = Zip.addEntryToArchive rootEntry Zip.emptyArchive
  BSL.writeFile outputPath (Zip.fromArchive archive)