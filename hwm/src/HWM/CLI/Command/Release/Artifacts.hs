{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Artifacts
  ( ReleaseArchiveOptions (..),
    parseCLI,
    runReleaseArchive,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.Text as T
import Data.Traversable (for)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format), Status (..), formatList, statusIcon, subPathSign)
import HWM.Core.Parsing (Parse (..), ParseCLI (..), parseLS)
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (fromEither)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (..), getArchiveConfigs)
import HWM.Domain.Environments (BuildEnvironment (..), Builder (..), getBuildEnvironment)
import HWM.Domain.Release (ArchiveFormat, ArtifactConfig (..), ReleaseArtifactConfigs, selectedArtifacts)
import HWM.Domain.Workspace (resolveWorkspaces)
import HWM.Integrations.Toolchain.Github (ensureIsLatestTag)
import HWM.Runtime.Archive (ArchiveInfo (..), ArchivingPlan (..), createArchive)
import HWM.Runtime.Network (getGHUploadUrl, uploadToGitHub)
import HWM.Runtime.Process (exec)
import HWM.Runtime.UI (forTable, indent, putLine, section, sectionTableM)
import Options.Applicative (argument, help, long, metavar, option, showDefault, str, strOption, switch, value)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, emptyPermissions, removeFile, removePathForcibly, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath (joinPath, (</>))

-- | Options for 'hwm release archive'
data ReleaseArchiveOptions = ReleaseArchiveOptions
  { targetName :: Maybe Text,
    ghPublish :: Bool,
    outputDir :: FilePath,
    ovFormat :: Maybe [Text],
    ovGhcOptions :: Maybe [Text],
    ovNameTemplate :: Maybe Text
  }
  deriving (Show)

instance ParseCLI ReleaseArchiveOptions where
  parseCLI =
    ReleaseArchiveOptions
      <$> optional (argument str (metavar "TARGET" <> help "Name of the release target to build (default: all)"))
      <*> switch (long "github" <> help "Upload generated artifacts to a GitHub Release")
      <*> strOption
        ( long "output-dir"
            <> metavar "OUTPUT_DIR"
            <> help "Directory to output the release artifacts."
            <> value defaultOutputDir
            <> showDefault
        )
      <*> optional (option (parseLS <$> str) (long "format" <> metavar "FORMAT" <> help "Override the archive format for the release target. Supported: zip, tar.gz."))
      <*> optional (option (parseLS <$> str) (long "ghc-options" <> metavar "GHC_OPTION" <> help "Override GHC options for the release target. Can be specified multiple times."))
      <*> optional (strOption (long "name-template" <> metavar "NAME_TEMPLATE" <> help "Override the name template for the release target. Use {name} and {version} as placeholders."))

genBindaryDir :: (MonadIO m, ToString a) => a -> m FilePath
genBindaryDir name = do
  let path = joinPath [".hwm/release/binaries", toString name]
  prepeareDir path
  pure path

ghcOptions :: [Text] -> [Text]
ghcOptions [] = []
ghcOptions xs = ["--ghc-options=" <> T.unwords xs]

defaultOutputDir :: FilePath
defaultOutputDir = ".hwm/dist"

prepeareDir :: (MonadIO m) => FilePath -> m ()
prepeareDir dir = liftIO $ do
  removePathForcibly dir
  createDirectoryIfMissing True dir

parseFormats :: [Text] -> ConfigT [ArchiveFormat]
parseFormats = fromEither "can't parse archive format" . traverse parse

withOverrides :: ReleaseArchiveOptions -> ReleaseArtifactConfigs -> ConfigT [(Name, ArtifactConfig)]
withOverrides ReleaseArchiveOptions {..} cfgs = do
  parsedFormats <- traverse parseFormats ovFormat
  map (second (applyOverrieds parsedFormats)) <$> selectedArtifacts targetName cfgs
  where
    applyOverrieds formats cfg =
      cfg
        { arcFormats = fromMaybe (arcFormats cfg) formats,
          arcGhcOptions = fromMaybe (arcGhcOptions cfg) ovGhcOptions,
          arcNameTemplate = fromMaybe (arcNameTemplate cfg) ovNameTemplate
        }

runReleaseArchive :: ReleaseArchiveOptions -> ConfigT ()
runReleaseArchive ops@ReleaseArchiveOptions {..} = do
  prepeareDir defaultOutputDir
  cfg <- asks config
  version <- asks (cfgVersion . config)
  cfgs <- getArchiveConfigs >>= withOverrides ops
  ghTag <- if ghPublish then Just <$> ensureIsLatestTag version else pure Nothing
  uploadUrl <- maybe (pure Nothing) (fmap Just . getGHUploadUrl cfg) ghTag
  builder <- buildBuilder <$> getBuildEnvironment Nothing
  sectionTableM
    "artifacts"
    [ ("destination", pure $ maybe (format outputDir) format uploadUrl),
      ("version", pure $ format version <> maybe "" (\tag -> " (GitHub Release " <> tag <> ")") ghTag),
      ("targets", pure $ formatList "," (map fst cfgs)),
      ("builder", pure $ format builder)
    ]

  plans <- forTable "build" cfgs (\x -> (fst x, buildPkg builder x))

  section "archive" $ pure ()
  artifacts <- for plans $ \(name, plan) -> do
    archives <- createArchive version plan
    indent 1
      $ section name
      $ for_ archives
      $ \ArchiveInfo {..} -> do
        putLine $ subPathSign <> format archivePath
        putLine $ subPathSign <> format sha256Path
    pure (name, archives)

  for_ uploadUrl $ \url -> section "publish (Github)"
    $ for_ artifacts
    $ \(name, archives) -> section name $ for_ archives $ \ArchiveInfo {..} -> do
      uploadToGitHub url archivePath
      putLine $ subPathSign <> format archivePath
      uploadToGitHub url sha256Path
      putLine $ subPathSign <> format sha256Path
  where
    buildPkg builder (name, ArtifactConfig {..}) = do
      binaryDir <- genBindaryDir name
      let (workspaceId, executableName) = second (T.drop 1) (T.breakOn ":" arcSource)
      optTarget <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
      Pkg {..} <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure optTarget
      genBinary builder pkgName binaryDir (ghcOptions arcGhcOptions)
      pure (statusIcon Checked, ArchivingPlan {nameTemplate = arcNameTemplate, outDir = outputDir, sourceDir = binaryDir, name = executableName, archiveFormats = arcFormats})

genBinary :: Builder -> PkgName -> FilePath -> [Text] -> ConfigT ()
genBinary builder pkgName dirPath args = do
  (success, buildOut) <- command
  unless success $ throwError (fromString $ "Build failed: " <> buildOut)
  when (builder == NixBuilder) (extractNixArtifact pkgName dirPath)
  where
    command = case builder of
      StackBuilder ->
        exec "stack" $ ["install", format pkgName, "--local-bin-path", format dirPath] <> args
      CabalBuilder ->
        exec "cabal"
          $ [ "install",
              format pkgName,
              "--install-method=copy",
              "--installdir",
              format dirPath,
              "--overwrite-policy=always"
            ]
          <> args
      NixBuilder ->
        -- WARNING: We DO NOT append 'args' here.
        -- Nix does not accept '--ghc-options' via CLI; it must be set in the flake.
        exec "nix" ["build", ".#" <> format pkgName, "-o", format (dirPath </> "result")]

extractNixArtifact :: PkgName -> FilePath -> ConfigT ()
extractNixArtifact pkgName distDir = do
  let resultLink = distDir </> "result"
      finalDest = distDir </> toString pkgName
      pkgStr = toString (format pkgName)

  liftIO $ createDirectoryIfMissing True distDir
  isLink <- liftIO $ doesPathExist resultLink
  unless isLink
    $ throwError
    $ fromString
    $ "Nix build completed, but did not create an output at: "
    <> resultLink
    <> "\n(This usually means the Nix derivation is empty or 'exec' hid a build failure.)"

  -- 3. Define potential binary locations within the Nix store path
  let searchPaths =
        [ resultLink </> "bin" </> pkgStr, -- Standard Haskell (Cabal/Stack)
          resultLink </> pkgStr, -- Simple/Single-binary derivation
          resultLink -- Derivation is the binary itself
        ]

  -- 4. Find the first path that actually exists
  maybeSource <- findM (liftIO . doesFileExist) searchPaths

  case maybeSource of
    Just sourcePath -> do
      liftIO $ copyFile sourcePath finalDest
      -- Ensure the user can execute it (Nix store is read-only)
      liftIO $ do
        let properPerms =
              setOwnerReadable True
                $ setOwnerWritable True
                $ setOwnerExecutable True emptyPermissions
        setPermissions finalDest properPerms
      -- Cleanup: Remove the 'result' symlink to keep the folder clean
      liftIO $ removeFile resultLink
    Nothing ->
      throwError
        $ fromString
        $ "Nix build succeeded, but binary '"
        <> pkgStr
        <> "' not found inside the Nix store path.\n"

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = do
  ifM (p x) (pure $ Just x) (findM p xs)