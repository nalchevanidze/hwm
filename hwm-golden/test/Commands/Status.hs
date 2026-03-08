module Commands.Status (testStatus) where

import Test.Hspec (Spec, describe, it)
import Utils.Golden (Golden (..), goldenTest)

testStatus :: Spec
testStatus = describe "status" $ do
  it "checks the status of a simple workspace" $
    goldenTest
      Golden
        { cmd = "status",
          project = "simple",
          scenario = "status/simple"
        }
  it "checks the status of a cabal-only workspace" $
    goldenTest
      Golden
        { cmd = "status",
          project = "morpheus",
          scenario = "status/cabal"
        }