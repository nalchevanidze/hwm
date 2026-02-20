{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.Platform (
  OS(..),
  Arch(..),
  Platform(..),
  detectPlatform,
  platformId,
  platformExt
) where

import Relude
import qualified Data.Text as Text
import Control.Exception (try, SomeException)
import Data.Char (toLower)
import qualified System.Info as Info
import qualified System.Environment as Env
import qualified System.Process as Proc

-- | OS type
data OS = MacOS | Linux | Windows | UnknownOS deriving (Eq, Show)
-- | Arch type
data Arch = X64 | Arm64 | UnknownArch deriving (Eq, Show)
-- | Platform
data Platform = Platform { os :: OS, arch :: Arch } deriving (Eq, Show)

platformId :: Platform -> Text
platformId (Platform MacOS X64) = "macos-x64"
platformId (Platform MacOS Arm64) = "macos-arm64"
platformId (Platform Linux X64) = "linux-x64"
platformId (Platform Linux Arm64) = "linux-arm64"
platformId (Platform Windows X64) = "windows-x64"
platformId (Platform Windows Arm64) = "windows-arm64"
platformId (Platform UnknownOS UnknownArch) = "unknown"
platformId (Platform os arch) =
  (case os of MacOS -> "macos"; Linux -> "linux"; Windows -> "windows"; UnknownOS -> "unknown")
  <> "-" <> (case arch of X64 -> "x64"; Arm64 -> "arm64"; UnknownArch -> "unknown")

platformExt :: Platform -> Text
platformExt (Platform Windows _) = ".exe"
platformExt _ = ""

-- | Detect platform using env vars, system calls, and Info
detectPlatform :: IO Platform
detectPlatform = do
  envOS <- Env.lookupEnv "RUNNER_OS"
  envArch <- Env.lookupEnv "RUNNER_ARCH"
  let osTag = fmap normalizeOS envOS <|> Just (normalizeOS Info.os)
      archTag = fmap normalizeArch envArch <|> Just (normalizeArch Info.arch)
  osFinal <- case osTag of
    Just "macos" -> pure MacOS
    Just "linux" -> pure Linux
    Just "windows" -> pure Windows
    _ -> do
      sysOS <- safeUname "-s"
      pure $ case sysOS of
        "Darwin" -> MacOS
        "Linux" -> Linux
        "Windows" -> Windows
        _ -> UnknownOS
  archFinal <- case archTag of
    Just "x64" -> pure X64
    Just "arm64" -> pure Arm64
    _ -> do
      sysArch <- safeUname "-m"
      pure $ case sysArch of
        "x86_64" -> X64
        "amd64" -> X64
        "arm64" -> Arm64
        "aarch64" -> Arm64
        _ -> UnknownArch
  pure $ Platform osFinal archFinal

normalizeOS :: String -> String
normalizeOS s = case map toLower s of
  "darwin" -> "macos"
  "macos" -> "macos"
  "linux" -> "linux"
  "windows" -> "windows"
  _ -> "unknown"

normalizeArch :: String -> String
normalizeArch s = case map toLower s of
  "x86_64" -> "x64"
  "x64" -> "x64"
  "amd64" -> "x64"
  "arm64" -> "arm64"
  "aarch64" -> "arm64"
  _ -> "unknown"

safeUname :: String -> IO String
safeUname flag = do
  result <- try (Proc.readProcess "uname" [flag] "") :: IO (Either SomeException String)
  pure $ either (const "unknown") (Text.unpack . Text.strip . Text.pack . toString) result
