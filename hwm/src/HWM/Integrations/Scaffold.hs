{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Scaffold (scaffoldPackage) where

import qualified Data.Text as T
import HWM.Core.Formatting (Status (..), StatusM)
import HWM.Core.Pkg (PkgName)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Integrations.Toolchain.Package (newPackage)
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

libHsTemplate :: Text
libHsTemplate =
  T.unlines
    [ "module Lib (someFunc) where",
      "",
      "someFunc :: IO ()",
      "someFunc = putStrLn \"Scaffolded by HWM\""
    ]

writeSource :: FilePath -> ConfigT (StatusM ConfigT)
writeSource targetDir = liftIO $ do
  createDirectoryIfMissing True (targetDir </> "src")
  writeFile (targetDir </> "src/Lib.hs") (T.unpack libHsTemplate)
  pure [("src", pure Updated)]

scaffoldPackage :: FilePath -> PkgName -> ConfigT (StatusM ConfigT)
scaffoldPackage targetDir pkgName = do
  sts <- writeSource targetDir
  fs <- map (second pure) <$> newPackage targetDir pkgName
  pure $ sts <> fs
