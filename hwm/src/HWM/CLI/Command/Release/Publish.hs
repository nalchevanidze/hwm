{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Release.Publish
  ( PublishOptions (..),
    runPublish,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Map as Map
import HWM.Core.Common (Name)
import HWM.Core.Formatting
  ( Color (..),
    Format (..),
    chalk,
    genMaxLen,
    padDots,
    statusIcon,
  )
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (Pkg (..), pkgId)
import HWM.Core.Result (Issue, Severity (..), maxSeverity)
import HWM.Domain.Config (Config (cfgRelease))
import HWM.Domain.ConfigT (ConfigT, Env (..), askVersion)
import HWM.Domain.Dependencies (sortByDependencyHierarchy)
import HWM.Domain.Release (Release (..))
import HWM.Domain.Workspace (WsPkgs, allPackages, resolveWsPkgs)
import HWM.Integrations.Toolchain.Package (deriveRegistry)
import HWM.Integrations.Toolchain.Stack (sdist, upload)
import HWM.Runtime.UI (printSummary, putLine, section, sectionTableM)
import Options.Applicative (argument, help, metavar, str)
import Relude hiding (intercalate)

failIssues :: [Issue] -> ConfigT ()
failIssues [] = pure ()
failIssues issues = do
  printSummary issues
  when (maxSeverity issues == Just SeverityError) $ liftIO exitFailure

newtype PublishOptions = PublishOptions
  { publishGroup :: Maybe Name
  }
  deriving (Show)

instance ParseCLI PublishOptions where
  parseCLI =
    PublishOptions
      <$> optional (argument str (metavar "GROUP" <> help "Name of the workspace group to publish (default: all)"))

arrangePackageRelease :: [Pkg] -> ConfigT [Pkg]
arrangePackageRelease pkgs = do
  (_, graph) <- allPackages >>= deriveRegistry
  sortByDependencyHierarchy graph pkgs

collectGroups :: Maybe Name -> ConfigT WsPkgs
collectGroups Nothing = do
  pbMap <- fromMaybe mempty . (>>= rlsPublish) <$> asks (cfgRelease . config)
  concat <$> traverse resolveWsPkgs (Map.elems pbMap)
collectGroups (Just name) = do
  pbMap <- fromMaybe mempty . (>>= rlsPublish) <$> asks (cfgRelease . config)
  case Map.lookup name pbMap of
    Just pbList -> resolveWsPkgs pbList
    Nothing -> throwError $ fromString $ toString $ "No publish configuration found for group \"" <> name <> "\". Check release configuration."

runPublish :: PublishOptions -> ConfigT ()
runPublish PublishOptions {..} = do
  wgs <- collectGroups publishGroup
  version <- askVersion
  when (null wgs) $ throwError "No publishable groups found. Check workspace group configuration."
  sectionTableM
    0
    "publish"
    [ ("version", pure $ chalk Magenta (format version)),
      ("target", pure $ chalk Cyan (fromMaybe "all" publishGroup)),
      ("registry", pure "hackage")
    ]

  pkgs <- arrangePackageRelease (concatMap snd wgs)
  let size = genMaxLen (map pkgId pkgs)
  section "publishing plan (topological sort)" $ do
    for_ (zip pkgs [1 ..] :: [(Pkg, Int)]) $ \(pkg, idx) -> do
      putLine $ "└── " <> padDots size (pkgId pkg) <> show idx

  issues <- traverse sdist (concatMap snd wgs)
  failIssues (concat issues)

  section "publishing" $ do
    for_ pkgs $ \pkg -> do
      (status, publishIssues) <- upload pkg
      putLine $ "└── " <> padDots size (pkgId pkg) <> statusIcon status
      failIssues publishIssues
