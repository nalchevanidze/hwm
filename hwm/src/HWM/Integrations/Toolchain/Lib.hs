{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Lib
  ( Library (..),
    updateDependencies,
    getBoundsDiffs,
    BoundsDiff,
    Libraries,
    MapDeps (..),
    LibPath,
    HasSourceDirs (..),
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
import qualified Data.Text as T
import GHC.Generics (Generic (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Domain.Bounds (Bounds)
import HWM.Domain.Config (getRule)
import HWM.Domain.ConfigT (ConfigT, config, pkgs)
import HWM.Domain.Dependencies
  ( Dependencies,
    Dependency (..),
    HasDependencies (..),
    fromDependencyList,
    toDependencyList,
  )
import HWM.Runtime.Files (aesonYAMLOptions)
import Relude

type Libraries = Map Name Library

type BoundsDiff = (Text, PkgName, Bounds, Bounds)

type LibPath = (Name, [Name])

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
processDependencies :: Pkg -> [Text] -> Dependencies -> (Dependency -> Maybe Bounds -> Maybe a) -> ConfigT [a]
processDependencies pkg path deps processor = go [] [] (toDependencyList deps)
  where
    scope = T.intercalate ":" path
    go results issues [] = do
      -- Inject accumulated dependency issues at the end
      unless (null issues)
        $ injectIssue
          Issue
            { issueTopic = pkgMemberId pkg,
              issueMessage = show (length issues) <> " dependency issue(s) in " <> scope,
              issueSeverity = SeverityWarning,
              issueDetails = Just DependencyIssue {issueDependencies = issues, issueFile = fromMaybe (cabalFile pkg) (hpackFile pkg)}
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

updateDependencies :: (Pkg, [Text]) -> Dependencies -> ConfigT Dependencies
updateDependencies (pkg, path) deps = do
  updated <- processDependencies pkg path deps $ \(Dependency depName depBounds) maybeExpected ->
    case maybeExpected of
      Nothing -> Just (Dependency depName depBounds) -- Preserve original when lookup fails
      Just expected -> Just (Dependency depName expected)
  -- Return updated dependencies using fromDependencyList
  pure $ fromDependencyList updated

getBoundsDiffs :: Pkg -> ([Text], Dependencies) -> ConfigT [BoundsDiff]
getBoundsDiffs pkg (path, deps) =
  processDependencies pkg path deps $ \(Dependency depName depBounds) maybeExpected ->
    case maybeExpected of
      Nothing -> Nothing -- Skip unknown dependencies in diff
      Just expected ->
        if depBounds == expected
          then Nothing
          else Just (T.intercalate ":" path, depName, depBounds, expected)

type DepsCtx = (Pkg, [Text])

class MapDeps a where
  mapDeps :: DepsCtx -> (DepsCtx -> Dependencies -> ConfigT Dependencies) -> a -> ConfigT a

instance MapDeps Dependencies where
  mapDeps ctx f = f ctx

instance MapDeps Library where
  mapDeps ctx f Library {..} = do
    newDependencies <- traverse (f ctx) dependencies
    pure $ Library {dependencies = newDependencies, ..}

instance (MapDeps a) => MapDeps (Map Text a) where
  mapDeps (pkg, path) f = Map.traverseWithKey (\name lib -> mapDeps (pkg, path <> [name]) f lib)

instance (MapDeps a) => MapDeps (Maybe a) where
  mapDeps ctx f = maybe (pure Nothing) (fmap Just . mapDeps ctx f)

instance HasDependencies Library where
  collectDependencies scope (Library {..}) = collectDependencies scope dependencies

class HasSourceDirs a where
  getSourceDirs :: (Text, [Text]) -> a -> [(Text, Name)]

instance (HasSourceDirs a) => HasSourceDirs (Maybe a) where
  getSourceDirs tag (Just l) = getSourceDirs tag l
  getSourceDirs _ Nothing = []

instance (HasSourceDirs a) => HasSourceDirs (Map Text a) where
  getSourceDirs (libType, tag) libs = concatMap (\(name, lib) -> getSourceDirs (libType, tag <> [name]) lib) (Map.toList libs)

instance HasSourceDirs Library where
  getSourceDirs (libType, tags) Library {..} = [(T.intercalate ":" (libType : tags), sourceDirs)]