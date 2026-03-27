module Main (main) where

import HWM.Golden (goldenSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec goldenSpec
