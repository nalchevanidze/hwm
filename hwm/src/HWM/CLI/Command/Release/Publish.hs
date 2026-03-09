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
    Status (Checked, Invalid),
    chalk,
    genMaxLen,
    padDots,
    statusIcon,
  )
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (..), maxSeverity)
import HWM.Domain.Config (Config (cfgRelease))
import HWM.Domain.ConfigT (ConfigT, Env (..), askVersion)
import HWM.Domain.Dependencies (sortByDependencyHierarchy)
import HWM.Domain.Release (Release (..))
import HWM.Domain.Workspace (WsPkgs, printPkgWSRef, resolveWsPkgs)
import HWM.Integrations.Toolchain.Cabal (nativeSdist)
import HWM.Integrations.Toolchain.Package (deriveDependencyGraph)
import HWM.Runtime.Network (uploadToHackage)
import HWM.Runtime.UI (printSummary, putLine, section, sectionTableM)
import Options.Applicative (argument, help, metavar, str)
import Relude hiding (intercalate)
import System.Directory (getCurrentDirectory)
import System.FilePath (makeRelative)

failIssues :: [Issue] -> ConfigT ()
failIssues [] = pure ()
failIssues issues
  | maxSeverity issues == Just SeverityError = do
      printSummary issues
      liftIO exitFailure
  | otherwise = traverse_ injectIssue issues

unpackPath :: (Pkg, Maybe FilePath) -> ConfigT (Pkg, FilePath)
unpackPath (pkg, Just path) = pure (pkg, path)
unpackPath (pkg, Nothing) = throwError $ fromString $ "No file path found for package " <> toString (printPkgWSRef pkg)

newtype PublishOptions = PublishOptions
  { publishGroup :: Maybe Name
  }
  deriving (Show)

instance ParseCLI PublishOptions where
  parseCLI =
    PublishOptions
      <$> optional (argument str (metavar "GROUP" <> help "Name of the release group to publish (default: all)"))

arrangePackageRelease :: [Pkg] -> ConfigT [Pkg]
arrangePackageRelease pkgs = do
  graph <- deriveDependencyGraph
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
    "publish"
    [ ("version", pure $ chalk Magenta (format version)),
      ("target", pure $ chalk Cyan (fromMaybe "all" publishGroup)),
      ("registry", pure "hackage")
    ]

  pkgs <- arrangePackageRelease (concatMap snd wgs)

  sdists <- traverse nativeSdist pkgs
  failIssues (concatMap snd sdists)
  releasePkgs <- traverse (unpackPath . fst) sdists
  cwd <- liftIO getCurrentDirectory

  let ls = zip releasePkgs [1 ..] :: [((Pkg, FilePath), Int)]

  let size = genMaxLen (map (\((pkg, _), n) -> show n <> ". " <> printPkgWSRef pkg) ls)

  section "publishing plan (topological sort)" $ do
    for_ ls $ \((pkg, filePath), idx) -> do
      putLine $ "└── " <> padDots size (show idx <> ". " <> printPkgWSRef pkg) <> fromString (makeRelative cwd filePath)

  section "publishing" $ do
    for_ releasePkgs $ \(pkg, filePath) -> do
      publishIssues <- uploadToHackage pkg filePath
      let status = if null publishIssues then Checked else Invalid
      putLine $ "└── " <> padDots size (printPkgWSRef pkg) <> statusIcon status
      failIssues publishIssues
