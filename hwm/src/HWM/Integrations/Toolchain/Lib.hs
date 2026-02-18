{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Lib
  ( Library (..),
    updateDependencies,
    updateLibrary,
    updateLibraries,
    checkDependencies,
    checkLibrary,
    checkLibraries,
    BoundsDiff,
    Libraries,
  )
where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (delete)
# else
import Data.HashMap.Lazy (delete)
#endif
import Control.Monad.Except (catchError)
import Data.Aeson.Types
  ( FromJSON (..),
    GFromJSON,
    Object,
    Parser,
    ToJSON (..),
    Value (..),
    Zero,
    genericParseJSON,
    genericToJSON,
    withObject,
  )
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Pkg (Pkg (pkgMemberId), PkgName, pkgYamlPath)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Domain.Bounds (Bounds)
import HWM.Domain.Config (getRule)
import HWM.Domain.ConfigT (ConfigT, config, pkgs)
import HWM.Domain.Dependencies (Dependencies, Dependency (..), fromDependencyList, toDependencyList)
import HWM.Runtime.Files (aesonYAMLOptions)
import Relude

type Libraries = Map Name Library

type BoundsDiff = (Text, PkgName, Bounds, Bounds)

data Library = Library
  { sourceDirs :: Name,
    dependencies :: Maybe Dependencies,
    __unknownFields :: Maybe Object
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Library where
  parseJSON = fromObject (\t o -> t {__unknownFields = o})

instance ToJSON Library where
  toJSON t = Object (toObject (genericToJSON aesonYAMLOptions t) <> fromMaybe mempty (__unknownFields t))

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

updateDependency :: PkgName -> ConfigT Bounds
updateDependency name = do
  cfg <- asks config
  pkgs <- asks pkgs
  getRule name pkgs cfg

-- | Process dependencies with error handling - shared logic for both check and update
processDependencies :: Pkg -> Text -> Dependencies -> (Dependency -> Maybe Bounds -> Maybe a) -> ConfigT [a]
processDependencies pkg scope deps processor = go [] [] (toDependencyList deps)
  where
    go results issues [] = do
      -- Inject accumulated dependency issues at the end
      unless (null issues)
        $ injectIssue
          Issue
            { issueTopic = pkgMemberId pkg,
              issueMessage = show (length issues) <> " dependency issue(s) in " <> scope,
              issueSeverity = SeverityWarning,
              issueDetails =
                Just
                  DependencyIssue
                    { issueDependencies = issues,
                      issueFile = pkgYamlPath pkg
                    }
            }
      pure (reverse results)
    go results issues (dep@(Dependency depName depBounds) : rest) = do
      result <- catchError (Just <$> updateDependency depName) (\_ -> pure Nothing)
      let (newIssues, maybeItem) = case result of
            Nothing -> ((scope, format depName, format depBounds, "unknown") : issues, processor dep Nothing)
            Just expected -> (issues, processor dep (Just expected))
      case maybeItem of
        Nothing -> go results newIssues rest
        Just item -> go (item : results) newIssues rest

updateDependencies :: Pkg -> Text -> Dependencies -> ConfigT Dependencies
updateDependencies pkg scope deps = do
  updated <- processDependencies pkg scope deps $ \(Dependency depName depBounds) maybeExpected ->
    case maybeExpected of
      Nothing -> Just (Dependency depName depBounds) -- Preserve original when lookup fails
      Just expected -> Just (Dependency depName expected)
  -- Return updated dependencies using fromDependencyList
  pure $ fromDependencyList updated


checkDependencies :: Pkg -> Text -> Dependencies -> ConfigT [BoundsDiff]
checkDependencies pkg scope deps =
  processDependencies pkg scope deps $ \(Dependency depName depBounds) maybeExpected ->
    case maybeExpected of
      Nothing -> Nothing -- Skip unknown dependencies in diff
      Just expected ->
        if depBounds == expected
          then Nothing
          else Just (scope, depName, depBounds, expected)

updateLibrary :: Pkg -> Text -> Library -> ConfigT Library
updateLibrary pkg scope Library {..} = do
  newDependencies <- traverse (updateDependencies pkg scope) dependencies
  pure $ Library {dependencies = newDependencies, ..}

checkLibrary :: Pkg -> Text -> Library -> ConfigT [BoundsDiff]
checkLibrary _ _ Library {dependencies = Nothing} = pure []
checkLibrary pkg scope Library {dependencies = Just deps} =
  checkDependencies pkg scope deps

updateLibraries :: Pkg -> Text -> Maybe Libraries -> ConfigT (Maybe Libraries)
updateLibraries _ _ Nothing = pure Nothing
updateLibraries pkg scope (Just libs) = do
  updated <- traverse (\(name, lib) -> (name,) <$> updateLibrary pkg (scope <> ":" <> name) lib) (Map.toList libs)
  pure $ Just $ Map.fromList updated

checkLibraries :: Pkg -> Text -> Libraries -> ConfigT [BoundsDiff]
checkLibraries pkg scope libs = concat <$> traverse step (Map.toList libs)
  where
    step (name, lib) = checkLibrary pkg (scope <> ":" <> name) lib
