{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Platform
  ( OS (..),
    Arch (..),
    Platform (..),
    detectPlatform,
    platformFilePath,
    toNixSystem,
  )
where

import Data.Char (toLower)
import HWM.Core.Formatting (Format (..))
import Relude
import qualified System.Environment as Env
import qualified System.Info as Info

-- | OS type
data OS = MacOS | Linux | Windows | UnknownOS deriving (Eq, Show)

-- | Arch type
data Arch = X64 | Arm64 | UnknownArch deriving (Eq, Show)

instance Format OS where
  format MacOS = "macos"
  format Linux = "linux"
  format Windows = "windows"
  format UnknownOS = "unknown"

instance Format Arch where
  format X64 = "x64"
  format Arm64 = "arm64"
  format UnknownArch = "unknown"

data Platform = Platform {os :: OS, arch :: Arch} deriving (Eq, Show)

instance Format Platform where
  format (Platform os' arch') = format os' <> "-" <> format arch'

platformFilePath :: Platform -> Text -> FilePath
platformFilePath (Platform Windows _) fileName = toString (fileName <> ".exe")
platformFilePath _ fileName = toString fileName

detectPlatform :: (MonadIO m) => m Platform
detectPlatform = liftIO $ do
  envOS <- Env.lookupEnv "RUNNER_OS"
  envArch <- Env.lookupEnv "RUNNER_ARCH"

  let finalOS = case envOS of
        Just val -> detectOS val
        Nothing -> detectOS Info.os

      finalArch = case envArch of
        Just val -> detectArch val
        Nothing -> detectArch Info.arch

  pure $ Platform finalOS finalArch

detectOS :: String -> OS
detectOS s = case map toLower s of
  "darwin" -> MacOS
  "macos" -> MacOS
  "mac os" -> MacOS
  "mac os x" -> MacOS
  "osx" -> MacOS
  "linux" -> Linux
  "linux-gnu" -> Linux
  "windows" -> Windows
  "windows_nt" -> Windows
  "win32" -> Windows
  "mingw32" -> Windows -- The crucial fix for GHC on Windows
  "mingw64" -> Windows
  "msys" -> Windows
  "cygwin" -> Windows
  _ -> UnknownOS

detectArch :: String -> Arch
detectArch s = case map toLower s of
  "x86_64" -> X64
  "x86-64" -> X64
  "x64" -> X64
  "amd64" -> X64
  "x86_64h" -> X64
  "arm64" -> Arm64
  "arm64e" -> Arm64
  "aarch64" -> Arm64 -- GHC uses aarch64 for Apple Silicon/Linux ARM
  "armv8" -> Arm64
  "armv8l" -> Arm64
  _ -> UnknownArch

toNixSystem :: Platform -> Text
toNixSystem (Platform Linux X64) = "x86_64-linux"
toNixSystem (Platform Linux Arm64) = "aarch64-linux"
toNixSystem (Platform MacOS X64) = "x86_64-darwin"
toNixSystem (Platform MacOS Arm64) = "aarch64-darwin"
toNixSystem (Platform Windows _) = "x86_64-windows" -- Note: Nix on Windows usually implies WSL, but this is the standard triple
toNixSystem _ = "unknown-system"
