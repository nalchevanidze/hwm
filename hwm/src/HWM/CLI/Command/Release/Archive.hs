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
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (..), getArchiveConfigs)
import HWM.Domain.Release (ArchiveConfig (..))
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), ArchiveOptions (..), createArchive)
import HWM.Runtime.Network (uploadToGitHub)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, short, showDefault, strOption, value)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (joinPath)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { targetName :: Maybe Text,
    ghPublishUrl :: Maybe Text,
    outputDir :: FilePath
  }
  -- opsTarget :: Maybe Name,
  -- opsFormat :: ArchiveFormat,
  -- opsGhcOptions :: [Text],
  -- opsNameTemplate :: Text
  -- outputDir :: FilePath

  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> optional (strOption (long "target" <> short 't' <> metavar "TARGET" <> help "Name of the release target to build. If not specified, all targets will be built."))
      <*> optional (strOption (long "gh-publish" <> short 'u' <> metavar "UPLOAD_URL" <> help "URL to upload the release artifact. If not specified, the artifact will not be uploaded."))
      <*> strOption
        ( long "output-dir"
            <> short 'o'
            <> metavar "OUTPUT_DIR"
            <> help "Directory to output the release artifacts."
            <> value ".hwm/dist"
            <> showDefault
        )

genBindaryDir :: (MonadIO m, ToString a) => a -> m FilePath
genBindaryDir name = do
  let path = joinPath [".hwm/release/binaries", toString name]
  prepeareDir path
  pure path

ghcOptions :: [Text] -> [Text]
ghcOptions [] = []
ghcOptions xs = ["--ghc-options=" <> T.unwords xs]

prepeareDir :: (MonadIO m) => FilePath -> m ()
prepeareDir dir = liftIO $ do
  removePathForcibly dir
  createDirectoryIfMissing True dir

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ReleaseArchiveOptions {..} = do
  prepeareDir outputDir
  version <- asks (cfgVersion . config)
  cfgs <- Map.toList <$> getArchiveConfigs
  for_ cfgs $ \(name, ArchiveConfig {..}) -> do
    binaryDir <- genBindaryDir name
    putLine $ "Building and extracting \"" <> name <> "\" ..."
    let (workspaceId, executableName) = second (T.drop 1) (T.breakOn ":" arcSource)
    targets <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
    Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure targets
    stackGenBinary pkgName binaryDir (ghcOptions arcGhcOptions)
    putLine "Compressing artifact..."
    archives <- createArchive version ArchiveOptions {nameTemplate = arcNameTemplate, outDir = outputDir, sourceDir = binaryDir, name = executableName, archiveFormats = arcFormats}

    for_ ghPublishUrl $ \uploadUrl -> do
      for_ archives $ \ArchiveInfo {..} -> do
        uploadToGitHub uploadUrl archivePath
        putLine "gh published"
        uploadToGitHub uploadUrl sha256Path
        putLine "checksum published"

    for_ archives $ \ArchiveInfo {..} -> do
      putLine $ "✅ Produced: " <> format archivePath