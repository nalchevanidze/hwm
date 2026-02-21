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
import HWM.Core.Formatting (Format (format))
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.ConfigT (ConfigT, getArchiveConfigs)
import HWM.Domain.Release (ArchiveConfig (..))
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), createZipArchive)
import HWM.Runtime.Network (uploadToGitHub)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, short, strOption)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (joinPath)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { targetName :: Maybe Text,
    ghPublishUrl :: Maybe Text
  }
  -- opsTarget :: Maybe Name,
  -- opsFormat :: ArchiveFormat,
  -- opsStrip :: Bool,
  -- opsNameTemplate :: Text

  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> optional (strOption (long "target" <> short 't' <> metavar "TARGET" <> help "Name of the release target to build. If not specified, all targets will be built."))
      <*> optional (strOption (long "gh-publish" <> short 'u' <> metavar "UPLOAD_URL" <> help "URL to upload the release artifact. If not specified, the artifact will not be uploaded."))

releaseDir :: FilePath
releaseDir = ".hwm/release"

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ReleaseArchiveOptions {..} = do
  cfgs <- Map.toList <$> getArchiveConfigs
  for_ cfgs $ \(name, ArchiveConfig {..}) -> do
    let localDir = joinPath [releaseDir, toString name]
    liftIO $ removePathForcibly localDir
    liftIO $ createDirectoryIfMissing True localDir
    putLine $ "Building and extracting \"" <> name <> "\" ..."
    let (workspaceId, executableName) = second (T.drop 1) (T.breakOn ":" arcSource)
    targets <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
    Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure targets
    stackGenBinary pkgName localDir
    putLine "Compressing artifact..."
    ArchiveInfo {..} <- createZipArchive localDir executableName "./"
    for_ ghPublishUrl $ \uploadUrl -> do
      uploadToGitHub uploadUrl zipPath
      putLine "gh published"

    putLine $ "✅ Produced: " <> format zipPath <> "\nHash: " <> format sha256
    pure ()