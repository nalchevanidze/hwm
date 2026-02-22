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
import qualified Data.Text as T
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
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue, Severity (..), maxSeverity)
import HWM.Domain.ConfigT (ConfigT, askVersion)
import HWM.Domain.Workspace (WorkGroup, Workspace, askWorkspaceGroups, canPublish, memberPkgs, selectGroup)
import HWM.Integrations.Toolchain.Stack (sdist, upload)
import HWM.Runtime.UI (printSummary, putLine, section, sectionTableM, sectionWorkspace)
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

collectGroups :: Maybe Name -> Workspace -> ConfigT [(Name, WorkGroup)]
collectGroups Nothing ws = pure $ filter (canPublish . snd) (Map.toList ws)
collectGroups (Just name) ws = do
  wg <- selectGroup name ws
  if canPublish wg
    then pure [(name, wg)]
    else throwError $ fromString $ toString $ "Target group \"" <> name <> "\" cannot be published. Check workspace group configuration."

runPublish :: PublishOptions -> ConfigT ()
runPublish PublishOptions {..} = do
  ws <- askWorkspaceGroups
  wgs <- collectGroups publishGroup ws
  version <- askVersion
  when (null wgs) $ throwError "No publishable groups found. Check workspace group configuration."

  sectionTableM
    0
    "publish"
    [ ("version", pure $ chalk Magenta (format version)),
      ("target", pure $ chalk Cyan (format (T.intercalate ", " (map fst wgs)))),
      ("registry", pure "hackage")
    ]

  issues <- traverse memberPkgs wgs >>= traverse sdist . concat
  failIssues (concat issues)

  sectionWorkspace $ for_ wgs $ \(name, g) ->
    section (chalk Bold name) $ do
      pkgs <- memberPkgs (name, g)
      for_ pkgs $ \pkg -> do
        (status, publishIssues) <- upload pkg
        putLine $ "└── " <> padDots (genMaxLen (map pkgMemberId pkgs)) (pkgMemberId pkg) <> statusIcon status
        failIssues publishIssues
