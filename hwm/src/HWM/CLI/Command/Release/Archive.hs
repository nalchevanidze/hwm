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
import HWM.Core.Formatting (Format (format), subPathSign)
import HWM.Core.Parsing (Parse (..), ParseCLI (..), parseLS)
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (fromEither)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (..), getArchiveConfigs)
import HWM.Domain.Release (ArtifactConfig (..), ArchiveFormat)
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), ArchiveOptions (..), createArchive)
import HWM.Runtime.Network (uploadToGitHub)
import HWM.Runtime.UI (putLine, section)
import Options.Applicative (help, long, metavar, option, showDefault, str, strOption, value)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (joinPath)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { targetName :: Maybe Text,
    ghPublishUrl :: Maybe Text,
    outputDir :: FilePath,
    ovFormat :: Maybe [Text],
    overrides :: ArchiveOverrides
  }
  deriving (Show)

data ArchiveOverrides = ArchiveOverrides
  { ovGhcOptions :: Maybe [Text],
    ovNameTemplate :: Maybe Text
  }
  deriving (Show)

instance ParseCLI ArchiveOverrides where
  parseCLI =
    ArchiveOverrides
      <$> optional (option (parseLS <$> str) (long "ghc-options" <> metavar "GHC_OPTION" <> help "Override GHC options for the release target. Can be specified multiple times."))
      <*> optional (strOption (long "name-template" <> metavar "NAME_TEMPLATE" <> help "Override the name template for the release target. Use {name} and {version} as placeholders."))

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> optional (strOption (long "target" <> metavar "TARGET" <> help "Name of the release target to build. If not specified, all targets will be built."))
      <*> optional (strOption (long "gh-publish" <> metavar "UPLOAD_URL" <> help "URL to upload the release artifact. If not specified, the artifact will not be uploaded."))
      <*> strOption
        ( long "output-dir"
            <> metavar "OUTPUT_DIR"
            <> help "Directory to output the release artifacts."
            <> value ".hwm/dist"
            <> showDefault
        )
      <*> optional (option (parseLS <$> str) (long "format" <> metavar "FORMAT" <> help "Override the archive format for the release target. Supported: zip, tar.gz."))
      <*> parseCLI

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

applyOverrieds :: Maybe [ArchiveFormat] -> ArchiveOverrides -> ArtifactConfig -> ArtifactConfig
applyOverrieds formats ArchiveOverrides {..} cfg =
  cfg
    { arcFormats = fromMaybe (arcFormats cfg) formats,
      arcGhcOptions = fromMaybe (arcGhcOptions cfg) ovGhcOptions,
      arcNameTemplate = fromMaybe (arcNameTemplate cfg) ovNameTemplate
    }

parseFormats :: [Text] -> ConfigT [ArchiveFormat]
parseFormats = fromEither "can't parse archive format" . traverse parse

withOverrides :: ReleaseArchiveOptions -> Map Name ArtifactConfig -> ConfigT [(Name, ArtifactConfig)]
withOverrides ReleaseArchiveOptions {..} cfgs = do
  parsedFormats <- traverse parseFormats ovFormat
  case targetName of
    Just target -> case Map.lookup target cfgs of
      Just cfg -> pure [(target, applyOverrieds parsedFormats overrides cfg)]
      Nothing -> throwError $ fromString $ "Target \"" <> toString target <> "\" not found in configuration."
    Nothing -> pure $ map (second (applyOverrieds parsedFormats overrides)) (Map.toList cfgs)

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ops@ReleaseArchiveOptions {..} = do
  prepeareDir outputDir
  version <- asks (cfgVersion . config)
  cfgs <- getArchiveConfigs >>= withOverrides ops
  for_ cfgs $ \(name, ArtifactConfig {..}) -> do
    binaryDir <- genBindaryDir name
    putLine $ "Building and extracting \"" <> name <> "\" ..."
    let (workspaceId, executableName) = second (T.drop 1) (T.breakOn ":" arcSource)
    targets <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
    Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure targets
    stackGenBinary pkgName binaryDir (ghcOptions arcGhcOptions)
    putLine "Compressing artifact..."
    archives <- createArchive version ArchiveOptions {nameTemplate = arcNameTemplate, outDir = outputDir, sourceDir = binaryDir, name = executableName, archiveFormats = arcFormats}

    putLine ""
    section name
      $ for_ archives
      $ \ArchiveInfo {..} -> do
        for_ ghPublishUrl $ \uploadUrl -> do
          uploadToGitHub uploadUrl archivePath
          putLine "gh published"
          uploadToGitHub uploadUrl sha256Path
          putLine "checksum published"

        putLine $ subPathSign <> format archivePath
        putLine $ subPathSign <> format sha256Path