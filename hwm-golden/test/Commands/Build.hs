module Commands.Build (testBuild) where

import HWM.Golden (Golden (..), goldenFailTest, goldenTest)
import Test.Hspec (Spec, describe, it)

testBuild :: Spec
testBuild = describe "build" $ do
  it "builds default environment/global scope" $
    goldenTest
      Golden
        { cmd = "build",
          project = "simple-bin",
          scenario = "build/default-global"
        }

  it "builds explicit environment" $
    goldenTest
      Golden
        { cmd = "build --env default",
          project = "simple-bin",
          scenario = "build/env-specific"
        }

  it "builds all environments" $
    goldenTest
      Golden
        { cmd = "build --env all",
          project = "simple-bin",
          scenario = "build/env-all"
        }

  it "builds workspace group scope" $
    goldenTest
      Golden
        { cmd = "build libs",
          project = "simple-bin",
          scenario = "build/scope-group"
        }

  it "builds workspace member scope" $
    goldenTest
      Golden
        { cmd = "build libs/foo",
          project = "simple-bin",
          scenario = "build/scope-member"
        }

  it "forwards fast flag for builder" $
    goldenTest
      Golden
        { cmd = "build --fast",
          project = "simple-bin",
          scenario = "build/fast"
        }

  it "rejects unknown workspace" $
    goldenFailTest
      Golden
        { cmd = "build ghosts",
          project = "simple-bin",
          scenario = "build/reject-unknown-workspace"
        }

  it "dispatches stack builder build shape" $
    goldenTest
      Golden
        { cmd = "build",
          project = "simple-bin",
          scenario = "build/stack-dispatch-shape"
        }

  it "dispatches nix builder build shape" $
    goldenTest
      Golden
        { cmd = "build",
          project = "simple-bin",
          scenario = "build/nix-dispatch-shape"
        }

  it "excludes package per environment configuration" $
    goldenTest
      Golden
        { cmd = "build libs",
          project = "simple-bin",
          scenario = "build/excluded-pkg"
        }
