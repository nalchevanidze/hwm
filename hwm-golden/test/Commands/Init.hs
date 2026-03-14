module Commands.Init (testInit) where

import HWM.Golden (Golden (..), goldenTest)
import Test.Hspec (Spec, describe, it)

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
