{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Root (ReleaseCommand (..), runRelease) where

import HWM.CLI.Command.Release.Archive (ReleaseArchiveOptions, runReleaseArchive)
import HWM.CLI.Command.Release.Publish (PublishOptions, runPublish)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (command, hsubparser, info, progDesc)
import Relude

-- | Top-level parser for 'release' command
data ReleaseCommand
  = ReleaseArchive ReleaseArchiveOptions
  | Publish PublishOptions
  deriving (Show)

instance ParseCLI ReleaseCommand where
  parseCLI =
    hsubparser
      ( command "archive" (info (ReleaseArchive <$> parseCLI) (progDesc "Build, archive, hash, and export a package release"))
          <> command "publish" (info (Publish <$> parseCLI) (progDesc "Publish a package release to Hackage"))
      )

runRelease :: ReleaseCommand -> ConfigT ()
runRelease (ReleaseArchive opts) = runReleaseArchive opts
runRelease (Publish opts) = runPublish opts