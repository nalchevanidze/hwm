module Main (main) where

import Commands.Build (testBuild)
import Commands.Environments (testEnvironments)
import Commands.Init (testInit)
import Commands.Install (testInstall)
import Commands.Release (testRelease)
import Commands.Run (testRun)
import Commands.Status (testStatus)
import Commands.Sync (testSync)
import Commands.Test (testTest)
import Commands.Version (testVersion)
import Commands.Workspace (testWorkspace)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  testInit
  testSync
  testStatus
  testRun
  testRelease
  testEnvironments
  testWorkspace
  testVersion
  testBuild
  testInstall
  testTest
