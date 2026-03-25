{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Hie
  ( syncHie,
  )
where

import Control.Monad.Except (catchError)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (format), Status (..))
import HWM.Core.Options (Options (..), askOptions)
import HWM.Core.Pkg (Pkg (..), pkgFile)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (injectIssue), Severity (..))
import HWM.Core.Sync (SyncMode (..))
import HWM.Domain.Build (Builder (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Domain.Workspace (allPackages)
import HWM.Integrations.Toolchain.Cabal (HasSourceDirs (..), readCabalPackage)
import HWM.Runtime.Files (readYaml, rewrite_)
import Relude
import System.Directory (doesFileExist)

-- -----------------------------------------------------------------------------
-- HIE file model
-- -----------------------------------------------------------------------------

data Component = Component
  { path :: FilePath,
    component :: Name
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show,
      Eq,
      Ord
    )

data Components = Components
  { stackYaml :: FilePath,
    components :: [Component]
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

data HieCradle = HieCradle
  { stack :: Maybe Components,
    cabal :: Maybe [Component]
  }
  deriving
    ( FromJSON,
      ToJSON,
      Generic,
      Show
    )

newtype HieConfig = HieConfig
  { cradle :: HieCradle
  }
  deriving
    ( FromJSON,
      ToJSON,
      Generic,
      Show
    )

packHie :: Builder -> Components -> HieConfig
packHie StackBuilder value = HieConfig {cradle = HieCradle {stack = Just value, cabal = Nothing}}
packHie CabalBuilder {} Components {components} = HieConfig {cradle = HieCradle {stack = Nothing, cabal = Just components}}
packHie NixBuilder Components {components} = HieConfig {cradle = HieCradle {stack = Nothing, cabal = Just components}}

-- -----------------------------------------------------------------------------
-- Public API
-- -----------------------------------------------------------------------------

syncHie :: SyncMode -> ConfigT Status
syncHie SyncModeSync = syncHieFile
syncHie SyncModeCheck = checkHieFile
syncHie SyncModeIgnore = pure Ignored

-- -----------------------------------------------------------------------------
-- Sync mode
-- -----------------------------------------------------------------------------

syncHieFile :: ConfigT Status
syncHieFile = do
  Options {..} <- askOptions
  BuildEnvironment {buildBuilder} <- getBuildEnvironment Nothing
  expectedComponents <- collectExpectedComponents
  issues <- validateExistingHie optionsHie buildBuilder optionsStack expectedComponents
  if null issues
    then pure Checked
    else rewrite_ optionsHie (const $ pure $ packHie buildBuilder Components {stackYaml = optionsStack, components = expectedComponents})

collectExpectedComponents :: ConfigT [Component]
collectExpectedComponents = do
  pkgs <- allPackages
  concat <$> traverse genComponents pkgs

genComponents :: Pkg -> ConfigT [Component]
genComponents pkg = map comp . getSourceDirs [format $ pkgName pkg] <$> readCabalPackage pkg
  where
    comp (tag, sourceDirs) = Component {path = "./" <> pkgFile pkg (toString sourceDirs), component = tag}

-- -----------------------------------------------------------------------------
-- Check mode
-- -----------------------------------------------------------------------------

checkHieFile :: ConfigT Status
checkHieFile = do
  Options {..} <- askOptions
  BuildEnvironment {buildBuilder} <- getBuildEnvironment Nothing
  expectedComponents <- collectExpectedComponents
  issues <- validateExistingHie optionsHie buildBuilder optionsStack expectedComponents
  if null issues
    then pure Checked
    else do
      traverse_ (reportHieError optionsHie) issues
      pure Invalid

reportHieError :: FilePath -> Text -> ConfigT ()
reportHieError hiePath message =
  injectIssue
    Issue
      { issueTopic = "hie.yaml",
        issueSeverity = SeverityError,
        issueMessage = message,
        issueDetails = Just (GenericIssue hiePath)
      }

validateExistingHie :: FilePath -> Builder -> FilePath -> [Component] -> ConfigT [Text]
validateExistingHie hiePath buildBuilder expectedStackYaml expectedComponents = do
  exists <- liftIO $ doesFileExist hiePath
  if not exists
    then pure ["hie.yaml is missing"]
    else do
      parsed <- (Right <$> (readYaml hiePath :: ConfigT HieConfig)) `catchError` (pure . Left)
      case parsed of
        Left issue -> pure ["Unable to parse hie.yaml: " <> issueMessage issue]
        Right hieConfig -> pure $ validateHieConfig buildBuilder expectedStackYaml expectedComponents hieConfig

validateHieConfig :: Builder -> FilePath -> [Component] -> HieConfig -> [Text]
validateHieConfig StackBuilder expectedStackYaml expectedComponents HieConfig {cradle = HieCradle {stack, cabal = _}} =
  case stack of
    Nothing -> ["hie.yaml cradle is not stack-based for the active stack environment"]
    Just Components {stackYaml, components} ->
      ["hie.yaml stackYaml mismatch: expected '" <> toText expectedStackYaml <> "', got '" <> toText stackYaml <> "'" | stackYaml /= expectedStackYaml]
        <> validateComponentDetails expectedComponents components
validateHieConfig CabalBuilder {inNixDevelopment = False} _ expectedComponents HieConfig {cradle = HieCradle {stack = _, cabal}} =
  case cabal of
    Nothing -> ["hie.yaml cradle is not cabal-based for the active cabal environment"]
    Just components -> validateComponentDetails expectedComponents components
validateHieConfig CabalBuilder {inNixDevelopment = True} _ expectedComponents HieConfig {cradle = HieCradle {stack = _, cabal}} =
  case cabal of
    Nothing -> ["hie.yaml cradle is not cabal-based for the active nix/cabal environment"]
    Just components -> validateComponentDetails expectedComponents components
validateHieConfig NixBuilder _ expectedComponents HieConfig {cradle = HieCradle {stack = _, cabal}} =
  case cabal of
    Nothing -> ["hie.yaml cradle is not cabal-based for the active nix environment"]
    Just components -> validateComponentDetails expectedComponents components

validateComponentDetails :: [Component] -> [Component] -> [Text]
validateComponentDetails expected actual =
  missingComponentErrors
    <> unknownComponentErrors
    <> wrongPathErrors
  where
    expectedByComponent = groupPathsByComponent expected
    actualByComponent = groupPathsByComponent actual

    expectedNames = Map.keysSet expectedByComponent
    actualNames = Map.keysSet actualByComponent

    missingComponentNames = sort $ Set.toList (Set.difference expectedNames actualNames)
    unknownComponentNames = sort $ Set.toList (Set.difference actualNames expectedNames)
    commonComponentNames = sort $ Set.toList (Set.intersection expectedNames actualNames)

    missingComponentErrors =
      [ "hie.yaml is missing components: " <> T.intercalate ", " missingComponentNames
      | not (null missingComponentNames)
      ]

    unknownComponentErrors =
      [ "hie.yaml has unknown components: " <> T.intercalate ", " unknownComponentNames
      | not (null unknownComponentNames)
      ]

    wrongPathErrors = mapMaybe componentPathMismatch commonComponentNames

    componentPathMismatch name = do
      expectedPaths <- Map.lookup name expectedByComponent
      actualPaths <- Map.lookup name actualByComponent
      guard (expectedPaths /= actualPaths)
      pure
        $ "hie.yaml component '"
        <> name
        <> "' has mismatched paths: expected "
        <> renderPathList expectedPaths
        <> ", got "
        <> renderPathList actualPaths

groupPathsByComponent :: [Component] -> Map Name (Set FilePath)
groupPathsByComponent = foldl' insert Map.empty
  where
    insert acc Component {component, path} =
      Map.insertWith Set.union component (Set.singleton path) acc

renderPathList :: Set FilePath -> Text
renderPathList paths =
  case sort (Set.toList paths) of
    [singlePath] -> "'" <> toText singlePath <> "'"
    xs -> "[" <> T.intercalate ", " (map toText xs) <> "]"
