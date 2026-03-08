{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Scaffold (scaffoldPackage) where

import qualified Data.Text as T
import HWM.Core.Formatting (StatusM)
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

scaffoldPackage :: FilePath -> PkgName -> ConfigT (StatusM ConfigT)
scaffoldPackage targetDir pkgName = do
  liftIO $ createDirectoryIfMissing True (targetDir </> "src")
  liftIO $ writeFile (targetDir </> "src/Lib.hs") (T.unpack libHsTemplate)
  map (second pure) <$> newPackage targetDir pkgName
