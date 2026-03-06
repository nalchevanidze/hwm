module Commands.Init (testInit) where

import Test.Hspec ( describe, it, Spec )
import Utils.Golden (Golden (..), goldenTest)

testInit :: Spec
testInit = describe "init" $ do
  it "inits a simple workspace" $
    goldenTest
      Golden
        { cmd = "init",
          project = "simple",
          scenario = "init/simple"
        }
  it "inits a huge monorepo" $
    goldenTest
      Golden
        { cmd = "init",
          project = "morpheus",
          scenario = "init/cabal"
        }
