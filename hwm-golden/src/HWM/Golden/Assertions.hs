{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Assertions
  ( diff,
    diffChanges,
    saveSnapshot,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HWM.Golden.Types (ChangeReport (..), ExpectedFiles (..))
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory, removePathForcibly)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.Hspec (expectationFailure)

ignoredDirs :: [FilePath]
ignoredDirs = [".hwm", ".stack-work", "dist-newstyle"]

ignoredFileSuffixes :: [Text]
ignoredFileSuffixes = [".log"]

diff :: FilePath -> IO ()
diff expectedDir = do
  expectedFiles <- collectFiles expectedDir
  actualFiles <- collectFiles "."

  let expectedMap = Map.fromList [(f, expectedDir </> f) | f <- expectedFiles]
  let actualMap = Map.fromList [(f, f) | f <- actualFiles]

  let added = sort (Map.keys (Map.difference actualMap expectedMap))
  let deleted = sort (Map.keys (Map.difference expectedMap actualMap))

  unless (null added && null deleted) $ do
    expectationFailure
      ( "Directory shape mismatch\n"
          <> "  Added: "
          <> show added
          <> "\n  Deleted: "
          <> show deleted
      )

  forM_ (sort (Map.keys (Map.intersection expectedMap actualMap))) $ \rel -> do
    let expectedFile = expectedDir </> rel
    let actualFile = rel
    compareFileContents rel expectedFile actualFile

saveSnapshot :: ChangeReport -> FilePath -> IO ()
saveSnapshot (ChangeReport (ExpectedFiles {added, modified}) _) dst = do
  whenM (doesDirectoryExist dst) $ removePathForcibly dst
  let filesToUpdate = added ++ modified
  unless (null filesToUpdate) $ do
    createDirectoryIfMissing True dst
    forM_ filesToUpdate $ \f -> do
      let srcPath = f
      let dstPath = dst </> f
      createDirectoryIfMissing True (takeDirectory dstPath)
      copyFile srcPath dstPath

diffChanges :: FilePath -> ChangeReport -> IO ()
diffChanges expectedDir (ChangeReport (ExpectedFiles {added, deleted, modified}) _) = do
  let filesToCompare = added ++ modified
  forM_ filesToCompare $ \f -> do
    let expectedFile = expectedDir </> f
    let actualFile = f
    compareFileContents f expectedFile actualFile

  forM_ deleted $ \f -> do
    exists <- doesPathExist f
    when exists
      $ expectationFailure
      $ "Idempotency failure: File should have been deleted but still exists: " <> f

collectFiles :: FilePath -> IO [FilePath]
collectFiles root = go ""
  where
    go rel = do
      let dir = if null rel then root else root </> rel
      entries <- listDirectory dir
      fmap concat $ forM (sort entries) $ \entry -> do
        let relPath = if null rel then entry else rel </> entry
        let absPath = root </> relPath
        isDir <- doesDirectoryExist absPath
        if isDir
          then if entry `elem` ignoredDirs then pure [] else go relPath
          else if shouldIgnoreFile relPath then pure [] else pure [relPath]

shouldIgnoreFile :: FilePath -> Bool
shouldIgnoreFile path =
  let name = toText (takeFileName path)
   in any (`T.isSuffixOf` name) ignoredFileSuffixes

compareFileContents :: FilePath -> FilePath -> FilePath -> IO ()
compareFileContents rel expectedFile actualFile = do
  expectedContent <- BS.readFile expectedFile
  actualContent <- BS.readFile actualFile
  when (expectedContent /= actualContent) $ do
    expectationFailure $
      "Content mismatch in "
        <> rel
        <> ":\n"
        <> renderContentDiff expectedContent actualContent

renderContentDiff :: BS.ByteString -> BS.ByteString -> String
renderContentDiff expected actual =
  let eLines = T.lines (TE.decodeUtf8With lenientDecode expected)
      aLines = T.lines (TE.decodeUtf8With lenientDecode actual)
   in case firstMismatch eLines aLines 1 of
        Nothing -> "Binary/content mismatch but no textual line diff available"
        Just (lineNo, e, a) ->
          "  First differing line: "
            <> show lineNo
            <> "\n  expected: "
            <> show e
            <> "\n  actual:   "
            <> show a

firstMismatch :: [Text] -> [Text] -> Int -> Maybe (Int, Text, Text)
firstMismatch [] [] _ = Nothing
firstMismatch [] (a : _) n = Just (n, "<EOF>", a)
firstMismatch (e : _) [] n = Just (n, e, "<EOF>")
firstMismatch (e : es) (a : as) n
  | e == a = firstMismatch es as (n + 1)
  | otherwise = Just (n, e, a)
