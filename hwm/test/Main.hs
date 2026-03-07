module Main (main) where

import Commands.Init (testInit)
import Commands.Sync (testSync)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  testInit
  testSync
