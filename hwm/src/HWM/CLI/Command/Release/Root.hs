{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Root (ReleaseCommand (..), runRelease) where

import HWM.CLI.Command.Release.Package (ReleasePackageOptions, runReleasePackage)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (command, hsubparser, info, progDesc)
import Relude

-- | Top-level parser for 'release' command
data ReleaseCommand 
    = ReleasePackage ReleasePackageOptions deriving (Show)

instance ParseCLI ReleaseCommand where
  parseCLI =
    hsubparser
      ( command "package" (info (ReleasePackage <$> parseCLI) (progDesc "Build, archive, hash, and export a package release"))
      )

runRelease :: ReleaseCommand -> ConfigT ()
runRelease (ReleasePackage opts) = runReleasePackage opts
