module Main (main) where

import qualified Commands.Sync
import Test.Hspec

main :: IO ()
main = hspec Commands.Sync.spec
