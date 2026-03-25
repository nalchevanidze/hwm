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
  it "syncs a simple workspace without package rewrites when targets.packages=ignore" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/simple-fix-ignore-packages"
        }
  it "checks cabal target without rewriting when targets.cabal=check" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-cabal-no-rewrite"
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
  it "checks hie target and reports errors when cradle type is invalid for stack" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-hie-stack-invalid"
        }
  it "checks hie target and accepts cabal cradle for nix builder" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-hie-nix-valid"
        }
  it "checks hie target and reports errors on missing/unknown components" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-hie-components-mismatch"
        }
  it "checks hie target and reports errors on wrong component path" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-hie-wrong-path"
        }
  it "checks stack hie target and reports errors on wrong component path" $
    goldenTest
      Golden
        { cmd = "sync",
          project = "simple",
          scenario = "sync/check-hie-stack-wrong-path"
        }
