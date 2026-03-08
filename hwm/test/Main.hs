module Main (main) where

import Commands.Init (testInit)
import Commands.Status (testStatus)
import Commands.Sync (testSync)
import Commands.Workspace (testWorkspace)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  testInit
  testSync
  testStatus
  testWorkspace
