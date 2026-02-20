{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HWM.CLI.Command.Release.Package (ReleasePackageOptions(..), parseCLI, runReleasePackage) where

import Relude
import Options.Applicative (Parser, strOption, long, metavar, help, optional)
import HWM.Core.Common (Name)
import HWM.Domain.ConfigT (ConfigT)

-- | Options for 'hwm release package'
data ReleasePackageOptions = ReleasePackageOptions
  { packageName :: Name
  , outFile :: Maybe FilePath
  } deriving (Show)

parseCLI :: Parser ReleasePackageOptions
parseCLI = ReleasePackageOptions
  <$> strOption (long "package" <> metavar "PACKAGE" <> help "Name of the package to release")
  <*> optional (strOption (long "out" <> metavar "FILE" <> help "Export resulting file paths to FILE"))

runReleasePackage :: ReleasePackageOptions -> ConfigT ()
runReleasePackage _opts = pure () -- TODO: implement native build, archive, hash, and export logic
