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
import HWM.Core.Parsing (Parse (..), ParseCLI (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (..), getArchiveConfigs)
import HWM.Domain.Release (ArchiveConfig (..), ArchiveFormat)
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Stack (stackGenBinary)
import HWM.Runtime.Archive (ArchiveInfo (..), ArchiveOptions (..), createArchive)
import HWM.Runtime.Network (uploadToGitHub)
import HWM.Runtime.UI (putLine, section)
import Options.Applicative (help, long, metavar, option, short, showDefault, str, strOption, value)
import Relude
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (joinPath)

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { targetName :: Maybe Text,
    ghPublishUrl :: Maybe Text,
    outputDir :: FilePath,
    overrides :: ArchiveOverrides
  }
  deriving (Show)

data ArchiveOverrides = ArchiveOverrides
  { ovFormat :: Maybe ArchiveFormat,
    ovGhcOptions :: Maybe [Text],
    ovNameTemplate :: Maybe Text
  }
  deriving (Show)

instance ParseCLI ArchiveOverrides where
  parseCLI =
    ArchiveOverrides
      <$> optional (option (str >>= parse) (long "format" <> short 'f' <> metavar "FORMAT" <> help "Override the archive format for the release target. Supported: zip, tar.gz."))
      <*> optional (some (strOption (long "ghc-option" <> short 'g' <> metavar "GHC_OPTION" <> help "Override GHC options for the release target. Can be specified multiple times.")))
      <*> optional (strOption (long "name-template" <> short 'n' <> metavar "NAME_TEMPLATE" <> help "Override the name template for the release target. Use {name} and {version} as placeholders."))

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

applyOverrieds :: ArchiveOverrides -> ArchiveConfig -> ArchiveConfig
applyOverrieds _ cfg = cfg

withOverrides :: ReleaseArchiveOptions -> Map Name ArchiveConfig -> ConfigT [(Name, ArchiveConfig)]
withOverrides ReleaseArchiveOptions {..} cfgs = do
  case targetName of
    Just target -> case Map.lookup target cfgs of
      Just cfg -> pure [(target, applyOverrieds overrides cfg)]
      Nothing -> throwError $ fromString $ "Target \"" <> toString target <> "\" not found in configuration."
    Nothing -> pure $ map (second (applyOverrieds overrides)) (Map.toList cfgs)

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ops@ReleaseArchiveOptions {..} = do
  prepeareDir outputDir
  version <- asks (cfgVersion . config)
  cfgs <- getArchiveConfigs >>= withOverrides ops
  for_ cfgs $ \(name, ArchiveConfig {..}) -> do
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