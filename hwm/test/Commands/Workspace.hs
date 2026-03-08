module Commands.Workspace (testWorkspace) where

import Test.Hspec (Spec, describe, it)
import Utils.Golden (Golden (..), goldenTest)

testWorkspace :: Spec
testWorkspace = describe "workspace" $ do
  it "workspace add" $
    goldenTest
      Golden
        { cmd = "workspace add libs/new",
          project = "simple",
          scenario = "workspace/add/simple"
        }
