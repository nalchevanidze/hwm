module Commands.Sync (testSync) where

import Test.Hspec ( describe, it, Spec )
import Utils.Golden (Golden (..), goldenTest)

testSync :: Spec
testSync = describe "sync" $ do
  it "syncs a simple workspace" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/simple"
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
