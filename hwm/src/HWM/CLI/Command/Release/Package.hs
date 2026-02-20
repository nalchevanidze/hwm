{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Package (ReleasePackageOptions (..), parseCLI, runReleasePackage) where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format))
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Integrations.Toolchain.Stack (runStack)
import HWM.Runtime.Platform (detectPlatform, platformExt)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strOption)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as Proc

-- | Options for 'hwm release package'
data ReleasePackageOptions = ReleasePackageOptions
  { packageName :: Name,
    outFile :: Maybe FilePath
  }
  deriving (Show)

instance ParseCLI ReleasePackageOptions where
  parseCLI =
    ReleasePackageOptions
      <$> strOption (long "package" <> metavar "PACKAGE" <> help "Name of the package to release")
      <*> optional (strOption (long "out" <> metavar "FILE" <> help "Export resulting file paths to FILE"))

releaseDir :: FilePath
releaseDir = ".hwm/release"

runReleasePackage :: ReleasePackageOptions -> ConfigT ()
runReleasePackage opts = do
  let pkgName = packageName opts
      outPath = outFile opts

  platform <- liftIO detectPlatform
  let binBase = toString pkgName
      ext = toString (platformExt platform)
      binName = binBase <> ext
      zipName = binBase <> "-" <> toString (format platform) <> ".zip"
      exactBinPath = releaseDir </> binName

  liftIO $ removePathForcibly releaseDir
  liftIO $ createDirectoryIfMissing True releaseDir

  putLine $ "Building and extracting " <> pkgName <> "..."

  (success, buildOut) <- runStack ["install", binBase, "--local-bin-path", releaseDir]
  unless success $ throwError (fromString $ "Build failed: " <> buildOut)
  binExists <- liftIO $ doesFileExist exactBinPath
  unless binExists $ throwError (fromString $ "Binary not found at expected path: " <> exactBinPath)

  putLine "Compressing artifact..."
  liftIO $ createZipArchive exactBinPath binName zipName

  -- TODO: Generate the real Hash (Placeholder for now, see next step)
  let hash = "sha256-placeholder"

  case outPath of
    Just file ->
      liftIO
        $ writeFile file
        $ toString
        $ T.unlines
          [ "HWM_ASSET_NAME=" <> format zipName,
            "HWM_BIN_PATH=" <> format exactBinPath,
            "HWM_ASSET_HASH=" <> hash
          ]
    Nothing -> pure ()

  putLine $ "✅ Produced: " <> format zipName <> "\nHash: " <> format hash
  pure ()