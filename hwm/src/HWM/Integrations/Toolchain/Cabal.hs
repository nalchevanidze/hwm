{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Cabal
  ( syncCabal,
    validateHackage,
  )
where

import Data.Foldable (Foldable (..))
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import HWM.Core.Formatting (Status (..))
import HWM.Core.Pkg (Pkg (..), cabalFilePath, pkgYamlPath)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Runtime.Files (remove)
import Hpack (Result (..), defaultOptions, hpackResult, setProgramName, setTarget)
import qualified Hpack as H
import Hpack.Config (ProgramName (..))
import Relude

-- | Translate Cabal warnings into formatting status for downstream reporting.
toStatus :: PackageCheck -> Status
toStatus p
  | isError p = Invalid
  | otherwise = Warning

isError :: PackageCheck -> Bool
isError PackageDistInexcusable {} = True
isError PackageBuildImpossible {} = True
isError PackageBuildWarning {} = False
isError PackageDistSuspiciousWarn {} = False
isError PackageDistSuspicious {} = False

validateHackage :: Pkg -> FilePath -> ConfigT [Status]
validateHackage pkg path = do
  gpd <- liftIO $ readGenericPackageDescription normal path
  let ls = checkPackage gpd Nothing
  for_ ls $ \l -> do
    injectIssue
      ( Issue
          { issueMessage = "Invalid package: " <> show l,
            issueSeverity = if isError l then SeverityError else SeverityWarning,
            issueTopic = pkgMemberId pkg,
            issueDetails = Just GenericIssue {issueFile = path}
          }
      )
  pure (map toStatus ls)

syncCabal :: Pkg -> ConfigT Status
syncCabal pkg = do
  remove (cabalFilePath pkg)
  let programName = ProgramName $ toString $ pkgName pkg
  let ops = setTarget (pkgYamlPath pkg) $ setProgramName programName defaultOptions
  Result {..} <- liftIO $ hpackResult ops
  ls <- validateHackage pkg resultCabalFile

  s <- case resultStatus of
    H.OutputUnchanged -> pure Checked
    _ -> pure Updated
  pure $ maximum (s : ls)
