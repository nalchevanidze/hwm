{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Archive
  ( ReleaseArchiveOptions (..),
    parseCLI,
    runReleaseArchive,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format))
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), createZipArchive)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strOption)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { opsPkgName :: Name,
    execName :: Maybe Name,
    outFile :: Maybe FilePath
  }
  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> strOption (long "pkg" <> metavar "PACKAGE" <> help "Name of the package to release")
      <*> optional (strOption (long "exec" <> metavar "EXECUTABLE" <> help "Name of the executable to release"))
      <*> optional (strOption (long "out" <> metavar "FILE" <> help "Export resulting file paths to FILE"))

releaseDir :: FilePath
releaseDir = ".hwm/release"

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ReleaseArchiveOptions {..} = do
  targets <- listToMaybe . concatMap snd <$> resolveWorkspaces [opsPkgName]
  Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> opsPkgName <> "\" not found in any workspace. Check package name and workspace configuration.") pure targets

  liftIO $ removePathForcibly releaseDir
  liftIO $ createDirectoryIfMissing True releaseDir

  putLine $ "Building and extracting " <> format pkgName <> "..."
  stackGenBinary pkgName releaseDir

  putLine "Compressing artifact..."

  ArchiveInfo {..} <- createZipArchive releaseDir (fromMaybe (format pkgName) execName) "./"

  for_ outFile $ \file ->
    liftIO
      $ writeFile file
      $ toString
      $ T.unlines
        [ "HWM_ASSET_NAME=" <> format zipPath,
          "HWM_BIN_NAME=" <> binName,
          "HWM_ASSET_HASH=" <> sha256
        ]

  putLine $ "✅ Produced: " <> format zipPath <> "\nHash: " <> format sha256
  pure ()