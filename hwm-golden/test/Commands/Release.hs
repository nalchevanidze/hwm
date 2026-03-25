module Commands.Release (testRelease) where

import HWM.Golden (Golden (..), goldenFailTest)
import Test.Hspec (Spec, describe, it)

testRelease :: Spec
testRelease = describe "release" $ do
  describe "artifacts" $ do
    it "rejects artifact when environments is explicitly empty" $
      goldenFailTest
        Golden
          { cmd = "release artifacts sample --builder=nix",
            project = "simple",
            scenario = "release/artifacts/reject-empty-environments"
          }

    it "rejects artifact when active environment is not listed" $
      goldenFailTest
        Golden
          { cmd = "release artifacts sample --builder=nix",
            project = "simple",
            scenario = "release/artifacts/reject-not-listed-environment"
          }

    it "allows omitted environments policy (then fails later on unsupported nix builder)" $
      goldenFailTest
        Golden
          { cmd = "release artifacts sample --builder=nix",
            project = "simple",
            scenario = "release/artifacts/allow-omitted-environments"
          }

  describe "publish" $ do
    it "rejects unknown publish group" $
      goldenFailTest
        Golden
          { cmd = "release publish ghost",
            project = "simple",
            scenario = "release/publish/reject-unknown-group"
          }
