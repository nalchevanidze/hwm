{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Package (ReleasePackageOptions (..), parseCLI, runReleasePackage) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Core.Result (MonadIssue(..))
import HWM.Runtime.UI (MonadUI(..))
import Options.Applicative (help, long, metavar, strOption)
import Relude
import HWM.Runtime.Platform (Platform(..), detectPlatform, platformId, platformExt)
import qualified System.Process as Proc
import qualified System.Exit as Exit
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

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

runReleasePackage :: ReleasePackageOptions -> ConfigT ()
runReleasePackage opts = do
  let pkgName = packageName opts
      outPath = outFile opts
  platform <- liftIO detectPlatform
  let binBase = toString pkgName
      ext = toString (platformExt platform)
      binName = binBase <> ext
      zipName = binBase <> "-" <> toString (platformId platform) <> ".zip"
  -- Build
  (success, buildOut) <- runStackLocal ["build", binBase]
  unless success $ injectIssue (fromString ("Build failed: " <> buildOut))
  -- Find built binary path
  binPath <- liftIO $ do
    (code, out, _) <- Proc.readProcessWithExitCode "stack" ["exec", "--", "which", binName] ""
    pure $ if code == Exit.ExitSuccess then T.strip (T.pack out) else ""
  -- Zip the binary using 7z
  zipResult <- liftIO $ do
    if T.null binPath
      then pure False
      else do
        (code, _out, _err) <- Proc.readProcessWithExitCode "7z" ["a", zipName, T.unpack binPath] ""
        pure (code == Exit.ExitSuccess)
  unless zipResult $ injectIssue (fromString ("Zip failed: " <> zipName))
  hash <- liftIO $ pure "sha256-placeholder" -- placeholder
  case outPath of
    Just file -> liftIO $ writeFile file (zipName <> "\n" <> binName <> "\n" <> hash <> "\n")
    Nothing -> pure ()
  _ <- uiWrite (T.pack ("Produced: " <> zipName <> "\nHash: " <> hash <> "\n"))
  pure ()
  where
    runStackLocal args = liftIO $ do
      (code, _out, err) <- Proc.readProcessWithExitCode "stack" args ""
      let success = code == Exit.ExitSuccess
      pure (success, err)
