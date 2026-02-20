{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Archive
  ( ReleaseArchiveOptions (..),
    parseCLI,
    runReleaseArchive,
  )
where

import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format))
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), createZipArchive)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strOption)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { pkgName :: Name,
    outFile :: Maybe FilePath
  }
  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> strOption (long "package" <> metavar "PACKAGE" <> help "Name of the package to release")
      <*> optional (strOption (long "out" <> metavar "FILE" <> help "Export resulting file paths to FILE"))

releaseDir :: FilePath
releaseDir = ".hwm/release"

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ReleaseArchiveOptions {..} = do
  liftIO $ removePathForcibly releaseDir
  liftIO $ createDirectoryIfMissing True releaseDir

  putLine $ "Building and extracting " <> pkgName <> "..."
  stackGenBinary pkgName releaseDir

  putLine "Compressing artifact..."
  ArchiveInfo {..} <- createZipArchive releaseDir pkgName "./"

  -- TODO: Generate the real Hash (Placeholder for now, see next step)
  let hash = "sha256-placeholder"

  case outFile of
    Just file ->
      liftIO
        $ writeFile file
        $ toString
        $ T.unlines
          [ "HWM_ASSET_NAME=" <> format zipPath,
            "HWM_BIN_NAME=" <> binName,
            "HWM_ASSET_HASH=" <> hash
          ]
    Nothing -> pure ()

  putLine $ "✅ Produced: " <> format zipPath <> "\nHash: " <> format hash
  pure ()