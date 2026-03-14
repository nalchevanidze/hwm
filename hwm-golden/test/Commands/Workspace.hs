module Commands.Workspace (testWorkspace) where

import HWM.Golden (Golden (..), goldenTest)
import Test.Hspec (Spec, describe, it)

testWorkspace :: Spec
testWorkspace = describe "workspace" $ do
  it "workspace add" $
    goldenTest
      Golden
        { cmd = "workspace add libs/new",
          project = "simple",
          scenario = "workspace/add/simple"
        }
