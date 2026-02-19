{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Scaffold (scaffoldPackage) where

import qualified Data.Text as T
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- A simple pure template function
packageYamlTemplate :: Text -> Text
packageYamlTemplate pkgName =
  T.unlines
    [ "name: " <> pkgName,
      "version: 0.1.0.0",
      "dependencies:",
      "  - base >= 4.7 && < 5",
      "library:",
      "  source-dirs: src"
    ]

libHsTemplate :: Text -> Text
libHsTemplate _ =
  T.unlines
    [ "module Lib (someFunc) where",
      "",
      "someFunc :: IO ()",
      "someFunc = putStrLn \"Scaffolded by HWM\""
    ]

-- The actual IO action HWM runs
scaffoldPackage :: (MonadIO m) => FilePath -> Text -> m ()
scaffoldPackage targetDir pkgName = do
  -- 1. Create the directories
  liftIO $ createDirectoryIfMissing True (targetDir </> "src")

  -- 2. Write the files
  liftIO $ writeFile (targetDir </> "package.yaml") (T.unpack $ packageYamlTemplate pkgName)
  liftIO $ writeFile (targetDir </> "src/Lib.hs") (T.unpack $ libHsTemplate pkgName)