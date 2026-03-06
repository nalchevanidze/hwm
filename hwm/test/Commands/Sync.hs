module Commands.Sync (spec) where

import Test.Hspec
import Utils.Golden (Golden (..), goldenTest)

spec :: Spec
spec = describe "hwm sync" $ do
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
