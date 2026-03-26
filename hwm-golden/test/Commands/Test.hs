module Commands.Test (testTest) where

import HWM.Golden (Golden (..), goldenFailTest, goldenTest)
import Test.Hspec (Spec, describe, it)

testTest :: Spec
testTest = describe "test" $ do
  it "tests default environment/global scope" $
    goldenTest
      Golden
        { cmd = "test",
          project = "simple-bin",
          scenario = "test/default-global"
        }

  it "tests all environments" $
    goldenTest
      Golden
        { cmd = "test --env all",
          project = "simple-bin",
          scenario = "test/env-all"
        }

  it "tests workspace group scope" $
    goldenTest
      Golden
        { cmd = "test libs",
          project = "simple-bin",
          scenario = "test/scope-group"
        }

  it "tests workspace member scope" $
    goldenTest
      Golden
        { cmd = "test libs/foo",
          project = "simple-bin",
          scenario = "test/scope-member"
        }

  it "forwards fast flag" $
    goldenTest
      Golden
        { cmd = "test --fast",
          project = "simple-bin",
          scenario = "test/fast"
        }

  it "rejects unknown workspace" $
    goldenFailTest
      Golden
        { cmd = "test ghosts",
          project = "simple-bin",
          scenario = "test/reject-unknown-workspace"
        }

  it "dispatches stack builder test shape" $
    goldenTest
      Golden
        { cmd = "test",
          project = "simple-bin",
          scenario = "test/stack-dispatch-shape"
        }

  it "dispatches nix builder test shape" $
    goldenTest
      Golden
        { cmd = "test",
          project = "simple-bin",
          scenario = "test/nix-dispatch-shape"
        }
