{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Version
  ( runVersion,
    VersionOptions,
  )
where

import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Core.Parsing (Parse (..), ParseCLI (parseCLI))
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (..))
import HWM.Core.Version (VersionChange (..), nextVersion)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.UI (putLine, sectionConfig, sectionTableM)
import Options.Applicative (argument, help, metavar)
import Options.Applicative.Builder (str)
import Relude

size :: Int
size = 16

newtype VersionOptions = VersionOptions {bump :: Maybe VersionChange} deriving (Show)

instance ParseCLI VersionOptions where
  parseCLI = VersionOptions <$> optional (argument (str >>= parse) (metavar "BUMP" <> help "Bump the version. Can be one of: major, minor, patch, or a specific version like 1.2.3"))

bumpVersion :: VersionChange -> Config -> ConfigT Config
bumpVersion (BumpVersion bump) Config {..} = do
  let version' = nextVersion bump cfgVersion
  sectionTableM
    size
    ("bump version (" <> format bump <> ")")
    [ ("from", pure $ format cfgVersion),
      ("to", pure $ chalk Cyan (format version'))
    ]
  pure Config {cfgVersion = version', cfgBounds = Nothing, ..}
bumpVersion (FixedVersion version') Config {..} = do
  sectionTableM
    size
    "set version"
    [ ("from", pure $ format cfgVersion),
      ("to", pure $ chalk Cyan (format version') <> if version' == cfgVersion then " (no change)" else "")
    ]

  when (version' < cfgVersion)
    $ injectIssue
      ( Issue
          { issueTopic = "version",
            issueSeverity = SeverityWarning,
            issueMessage = "The specified version is lower than the current version. This may cause issues with package managers and should be done with caution.",
            issueDetails = Nothing
          }
      )

  pure Config {cfgVersion = version', ..}

runVersion :: VersionOptions -> ConfigT ()
runVersion (VersionOptions (Just bump)) = (bumpVersion bump `updateConfig`) $ do
  sectionConfig size [("hwm.yaml", pure $ chalk Green "✓")]
  syncPackages
runVersion (VersionOptions Nothing) = do
  putLine . format . cfgVersion =<< asks config
  exitSuccess
