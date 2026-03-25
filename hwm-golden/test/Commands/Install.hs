module Commands.Install (testInstall) where

import HWM.Golden (Golden (..), goldenFailTest, goldenTest)
import Test.Hspec (Spec, describe, it)

testInstall :: Spec
testInstall = describe "install" $ do
  it "installs default environment/global scope" $
    goldenTest
      Golden
        { cmd = "install",
          project = "simple-bin",
          scenario = "install/default-global"
        }

  it "installs workspace member scope" $
    goldenTest
      Golden
        { cmd = "install libs/foo",
          project = "simple-bin",
          scenario = "install/scope-member"
        }

  it "forwards fast flag" $
    goldenTest
      Golden
        { cmd = "install --fast",
          project = "simple-bin",
          scenario = "install/fast"
        }

  it "uses stack local-bin-path install shape" $
    goldenTest
      Golden
        { cmd = "install",
          project = "simple-bin",
          scenario = "install/stack-local-bin-path"
        }

  it "uses cabal install args" $
    goldenTest
      Golden
        { cmd = "install",
          project = "simple-bin",
          scenario = "install/cabal-install-args"
        }

  it "rejects install with nix builder" $
    goldenFailTest
      Golden
        { cmd = "install",
          project = "simple-bin",
          scenario = "install/reject-nix-builder"
        }

  it "rejects install with nix/cabal development mode" $
    goldenFailTest
      Golden
        { cmd = "install",
          project = "simple-bin",
          scenario = "install/reject-nix-cabal-builder"
        }
