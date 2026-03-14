module Commands.Sync (testSync) where

import HWM.Golden (Golden (..), goldenTest)
import Test.Hspec (Spec, describe, it)

testSync :: Spec
testSync = describe "sync" $ do
  it "syncs a simple workspace(no fix, as there is no registry)" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/simple"
        }
  it "syncs a simple workspace(should fix)" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/simple-fix"
        }
  it "syncs a cabal-only workspace" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "morpheus",
          scenario = "sync/cabal"
        }
  it "syncs a nix workspace" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "morpheus",
          scenario = "sync/nix"
        }
  it "syncs a stack workspace" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "morpheus",
          scenario = "sync/stack"
        }
