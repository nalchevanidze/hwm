{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Stack
  ( Stack (..),
    syncStackYaml,
    createEnvYaml,
    stackPath,
    sdist,
    upload,
    parseExtraDeps,
    scanStackFiles,
    buildMatrix,
    runStack,
  )
where

import Control.Monad.Except
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Map as Map
import Data.Text (pack)
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), Status (..), indentBlockNum, slugify)
import HWM.Core.Options (Options (..), askOptions)
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (Pkg (..), PkgName, pkgId, pkgYamlPath)
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..), fromEither)
import HWM.Core.Version (Version, parseGHCVersion)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Matrix (BuildEnv (..), BuildEnvironment (..), Matrix (..), getBuildEnvironment, hkgRefs)
import HWM.Runtime.Cache (getSnapshotGHC)
import HWM.Runtime.Files (aesonYAMLOptions, readYaml, rewrite_)
import HWM.Runtime.Logging (log)
import Relude hiding (head, tail)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>))
import System.FilePath.Glob (compile, globDir1)
import System.FilePath.Posix (takeFileName)
import System.Process (readProcessWithExitCode)

data Stack = Stack
  { packages :: [FilePath],
    resolver :: Name,
    allowNewer :: Maybe Bool,
    saveHackageCreds :: Maybe Bool,
    extraDeps :: Maybe [Name],
    compiler :: Maybe Text
  }
  deriving
    ( Show,
      Generic
    )

type VersionRegistry = Map PkgName Version

instance FromJSON Stack where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Stack where
  toJSON = genericToJSON aesonYAMLOptions

parseExtraDeps :: (MonadError Issue m) => [Text] -> m (Maybe VersionRegistry)
parseExtraDeps [] = pure Nothing
parseExtraDeps entries = do
  parsed <- traverse parseExtraDep entries
  if null parsed then throwError "No valid extra dependencies found" else pure $ Just $ Map.fromList parsed

parseExtraDep :: (MonadError Issue m) => Text -> m (PkgName, Version)
parseExtraDep entry = do
  let (namePart, versionPart) = T.breakOnEnd "-" entry
      segment = T.dropEnd 1 namePart
  when (T.null segment || T.null versionPart)
    $ throwError "Invalid extra-dep format: missing package segment or version part"
  pkgName <- fromEither ("Invalid package name: " <> segment) (parse segment)
  version <- fromEither ("Invalid version: " <> versionPart) (parse versionPart)
  pure (pkgName, version)

syncStackYaml :: ConfigT ()
syncStackYaml = do
  stackYamlPath <- stack <$> askOptions
  rewrite_ stackYamlPath $ const $ do
    BuildEnvironment {buildPkgs, buildEnv = BuildEnv {..}} <- getBuildEnvironment Nothing
    pure
      Stack
        { saveHackageCreds = Just False,
          extraDeps = map format . sort . hkgRefs <$> extraDeps,
          packages = map pkgDirPath buildPkgs,
          compiler = Nothing,
          ..
        }

stackPath :: Maybe Name -> ConfigT FilePath
stackPath (Just name) = pure $ ".hwm/matrix/stack-" <> toString name <> ".yaml"
stackPath Nothing = stack <$> askOptions

createEnvYaml :: Name -> ConfigT ()
createEnvYaml target = do
  path <- stackPath (Just target)
  liftIO $ createDirectoryIfMissing True ".hwm/matrix/"
  rewrite_ path $ const $ do
    BuildEnvironment {buildEnv = BuildEnv {..}, ..} <- getBuildEnvironment (Just target)
    pure
      Stack
        { saveHackageCreds = Just False,
          extraDeps = map format . sort . hkgRefs <$> buildExtraDeps,
          packages = map (("../../" <>) . pkgDirPath) buildPkgs,
          compiler = Nothing,
          ..
        }

runStack :: [String] -> ConfigT (Bool, String)
runStack args = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" args "")
  case code of
    ExitSuccess {} -> pure (True, out)
    ExitFailure {} -> pure (False, out)

sdist :: Pkg -> ConfigT [Issue]
sdist pkg = do
  let issueTopic = pkgMemberId pkg
      issueMessage = "stack sdist detected Issues. No packages were published."
  (isSuccess, out) <- runStack ["sdist", toString (pkgName pkg)]
  let severity = if isSuccess then findIssue out else Just SeverityError
  case severity of
    Nothing -> pure []
    Just issueSeverity -> do
      issueFile <- log "sdist" [("COMMAND", "stack sdist " <> format (pkgName pkg)), ("SEVERITY", show issueSeverity)] (pack out)
      let issueDetails = Just GenericIssue {issueFile}
       in pure [Issue {..}]

upload :: Pkg -> ConfigT (Status, [Issue])
upload pkg = do
  (isSuccess, out) <- runStack ["upload", toString (pkgName pkg)]
  ( if isSuccess
      then pure (Checked, [])
      else
        ( do
            pure
              ( Invalid,
                [ Issue
                    { issueTopic = pkgMemberId pkg,
                      issueMessage = "Package publishing failed:" <> indentBlockNum 4 ("\n\n" <> T.pack out),
                      issueSeverity = SeverityError,
                      issueDetails = Just GenericIssue {issueFile = pkgYamlPath pkg}
                    }
                ]
              )
        )
    )

findIssue :: String -> Maybe Severity
findIssue str =
  let ls = map T.strip $ T.lines $ T.toLower $ T.pack str
   in case find ("error:" `T.isInfixOf`) ls of
        Just _ -> Just SeverityError
        Nothing -> case find ("warning:" `T.isInfixOf`) ls of
          Just _ -> Just SeverityWarning
          Nothing -> Nothing

scanStackFiles :: (MonadIO m, MonadError Issue m) => Options -> FilePath -> m (NonEmpty (Name, Stack))
scanStackFiles opts root = do
  let defaultPath = root </> stack opts
  defaultExists <- liftIO $ doesFileExist defaultPath
  variantPaths <- liftIO $ globDir1 (compile "stack-*.yaml") root
  stacks <- traverse loadEnv ([defaultPath | defaultExists] <> variantPaths)
  case stacks of
    [] -> throwError "No stack.yaml found in current directory. Run 'stack init' first or ensure you're in a Stack project"
    (defaultEnv : envs) -> pure (defaultEnv :| envs)
  where
    loadEnv path = do
      seConfig <- readYaml path
      let stackName = fromMaybe "default" (deriveEnviromentName path)
      pure (stackName, seConfig)

deriveEnviromentName :: FilePath -> Maybe Text
deriveEnviromentName path = slugify <$> T.stripPrefix "stack-" (toText (dropExtension (takeFileName path)))

buildMatrix :: (MonadIO m, MonadError Issue m) => [Pkg] -> NonEmpty (Name, Stack) -> m Matrix
buildMatrix pkgs (defaultEnv :| envs) = do
  environments <- sortOn ghc <$> traverse (inferBuildEnv pkgs) (defaultEnv : envs)
  pure Matrix {defaultEnvironment = fst defaultEnv, environments}

inferBuildEnv :: (MonadIO m, MonadError Issue m) => [Pkg] -> (Name, Stack) -> m BuildEnv
inferBuildEnv allPkgs (name, Stack {extraDeps = deps, ..}) = do
  ghc <- maybe (getSnapshotGHC resolver) (fromEither "GHC Parsing" . parseGHCVersion) compiler
  extraDeps <- parseExtraDeps (fromMaybe [] deps)
  let excludeList = filter ((`notElem` packages) . pkgDirPath) allPkgs
      exclude = if null excludeList then Nothing else Just (map pkgId excludeList)
  pure BuildEnv {..}
