{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Root (ReleaseCommand (..), runRelease) where

import HWM.Core.Parsing (ParseCLI (..))
import Options.Applicative (Parser, command, hsubparser, info, progDesc)
import Relude

-- | Top-level parser for 'release' command
data ReleaseCommand = ReleasePackage Package.ReleasePackageOptions deriving (Show)

parseCLI :: Parser ReleaseCommand
parseCLI =
  hsubparser
    ( command "package" (info (ReleasePackage <$> parseCLI) (progDesc "Build, archive, hash, and export a package release"))
    )

runRelease :: ReleaseCommand -> ConfigT ()
runRelease (ReleasePackage opts) = runReleasePackage opts
