module Main (main) where

import Commands.Environments (testEnvironments)
import Commands.Init (testInit)
import Commands.Release (testRelease)
import Commands.Run (testRun)
import Commands.Status (testStatus)
import Commands.Sync (testSync)
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
