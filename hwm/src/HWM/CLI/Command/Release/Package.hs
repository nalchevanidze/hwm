{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Package (ReleasePackageOptions (..), parseCLI, runReleasePackage) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (help, long, metavar, strOption)
import Relude

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
runReleasePackage _opts = pure () -- TODO: implement native build, archive, hash, and export logic
