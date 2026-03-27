{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Changes (trackChanges) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
#ifndef mingw32_HOST_OS
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
#endif
import qualified Data.Yaml as Yaml
import HWM.Golden.Types (ChangeReport (..), ExpectedFiles (..))
import Relude
import System.Directory (doesDirectoryExist, doesPathExist, getModificationTime, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif

managed :: [String]
managed = [".cabal", ".yaml", ".nix", ".project"]

ignoredDirs :: [FilePath]
ignoredDirs = [".hwm", ".stack-work", "dist-newstyle"]

findManagedFiles :: FilePath -> IO [FilePath]
findManagedFiles dir = do
  contents <- listDirectory dir
  paths <-
    mapM
      ( \path -> do
          let p = dir </> path
          isDir <- doesDirectoryExist p
          pure (p, isDir)
      )
      contents
  let files = [p | (p, isDir) <- paths, not isDir, takeExtension p `elem` managed]
  subDirFiles <- concat <$> mapM collect paths
  pure (files ++ subDirFiles)
  where
    collect (p, isDir)
      | not isDir = pure []
      | takeFileName p `elem` ignoredDirs = pure []
      | otherwise = findManagedFiles p

canonicalPath :: FilePath -> FilePath
canonicalPath p = toString (fromMaybe (toText p) (T.stripPrefix "./" (toText p)))

hashBytes :: BS.ByteString -> Word64
hashBytes = BS.foldl' fnvStep fnvOffset
  where
    fnvOffset = 14695981039346656037
    fnvPrime = 1099511628211
    fnvStep h b = (h `xor` fromIntegral b) * fnvPrime

data FileFingerprint = FileFingerprint
  { fpMTime :: UTCTime,
    fpCTime :: Maybe UTCTime,
    fpSize :: Word64,
    fpHash :: Word64
  }

snapshotManagedFiles :: IO (Map.Map FilePath FileFingerprint)
snapshotManagedFiles = do
  files <- findManagedFiles "."
  fmap Map.fromList $ forM files $ \path -> do
    mTime <- getModificationTime path
    content <- BS.readFile path
#ifdef mingw32_HOST_OS
    let cTime = Nothing
        size = fromIntegral (BS.length content)
#else
    st <- Posix.getFileStatus path
    let cTime = Just (posixSecondsToUTCTime (realToFrac (Posix.statusChangeTimeHiRes st)))
        size = fromIntegral (Posix.fileSize st)
#endif
    pure
      ( path,
        FileFingerprint
          { fpMTime = mTime,
            fpCTime = cTime,
            fpSize = size,
            fpHash = hashBytes content
          }
      )

buildChangeReport :: Map.Map FilePath FileFingerprint -> Map.Map FilePath FileFingerprint -> ChangeReport
buildChangeReport oldMap newMap =
  let added = sort (map canonicalPath (Map.keys (Map.difference newMap oldMap)))
      deleted = sort (map canonicalPath (Map.keys (Map.difference oldMap newMap)))
      common = Map.intersectionWith (,) oldMap newMap
      wasTouched FileFingerprint {fpMTime = oldM, fpCTime = oldC, fpSize = oldS} FileFingerprint {fpMTime = newM, fpCTime = newC, fpSize = newS} =
        oldM /= newM || oldC /= newC || oldS /= newS
      modified =
        sort
          [ canonicalPath path
          | (path, (oldFp, newFp)) <- Map.toList common,
            fpHash oldFp /= fpHash newFp
          ]
      touched =
        sort
          [ canonicalPath path
          | (path, (oldFp, newFp)) <- Map.toList common,
            fpHash oldFp == fpHash newFp,
            wasTouched oldFp newFp
          ]
   in ChangeReport ExpectedFiles {..} Nothing

loadInvocations :: IO (Maybe Value)
loadInvocations = do
  file <- fromMaybe ".hwm/invocations.yaml" <$> lookupEnv "HWM_GOLDEN_INVOCATIONS"
  exists <- doesPathExist file
  if not exists
    then pure Nothing
    else do
      parsed <- Yaml.decodeFileEither file
      case parsed of
        Right (Object obj) -> pure (KM.lookup (K.fromText "calls") obj <|> Just (Object obj))
        Right v -> pure (Just v)
        Left _ -> pure Nothing

trackChanges :: IO a -> IO (ChangeReport, a)
trackChanges action = do
  oldState <- snapshotManagedFiles
  a <- action
  newState <- snapshotManagedFiles
  inv <- loadInvocations
  pure ((buildChangeReport oldState newState) {calls = inv}, a)
