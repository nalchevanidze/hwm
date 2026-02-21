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
import qualified Data.Map as Map
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format))
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.ConfigT (ConfigT, getArchiveConfigs)
import HWM.Domain.Release (ArchiveConfig (..))
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), createZipArchive)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strOption)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { opsPkgName :: Maybe Name,
    execName :: Maybe Name,
    outFile :: Maybe FilePath
  }
  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> optional (strOption (long "pkg" <> metavar "PACKAGE" <> help "Name of the package to release"))
      <*> optional (strOption (long "exec" <> metavar "EXECUTABLE" <> help "Name of the executable to release"))
      <*> optional (strOption (long "out" <> metavar "FILE" <> help "Export resulting file paths to FILE"))

releaseDir :: FilePath
releaseDir = ".hwm/release"

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ReleaseArchiveOptions {..} = do
  liftIO $ removePathForcibly releaseDir
  liftIO $ createDirectoryIfMissing True releaseDir

  cfgs <- Map.toList <$> getArchiveConfigs

  for_ cfgs $ \(name, ArchiveConfig {..}) -> do
    putLine $ "Building and extracting \"" <> name <> "\" ..."
    let (workspaceId, executableName) = T.breakOn ":" arcSource
    targets <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
    Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure targets

    stackGenBinary pkgName releaseDir

    putLine "Compressing artifact..."

    ArchiveInfo {..} <- createZipArchive releaseDir executableName "./"

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