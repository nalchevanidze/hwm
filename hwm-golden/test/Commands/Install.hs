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
