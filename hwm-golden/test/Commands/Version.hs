module Commands.Version (testVersion) where

import HWM.Golden (Golden (..), goldenFailTest, goldenTest)
import Test.Hspec (Spec, describe, it)

testVersion :: Spec
testVersion = describe "version" $ do
  it "shows current version" $
    goldenTest
      Golden
        { cmd = "version",
          project = "version-clean",
          scenario = "version/show-current"
        }

  it "bumps patch version" $
    goldenTest
      Golden
        { cmd = "version patch",
          project = "version-clean",
          scenario = "version/bump-patch"
        }

  it "bumps minor version" $
    goldenTest
      Golden
        { cmd = "version minor",
          project = "version-clean",
          scenario = "version/bump-minor"
        }

  it "bumps major version" $
    goldenTest
      Golden
        { cmd = "version major",
          project = "version-clean",
          scenario = "version/bump-major"
        }

  it "sets fixed higher version" $
    goldenTest
      Golden
        { cmd = "version 0.2.0",
          project = "version-clean",
          scenario = "version/set-fixed-higher"
        }

  it "sets fixed same version" $
    goldenTest
      Golden
        { cmd = "version 0.1.0",
          project = "version-clean",
          scenario = "version/set-fixed-same"
        }

  it "sets lower fixed version with warning" $
    goldenTest
      Golden
        { cmd = "version 0.0.9",
          project = "version-clean",
          scenario = "version/set-fixed-lower-warning"
        }

  it "rejects invalid bump token" $
    goldenFailTest
      Golden
        { cmd = "version banana",
          project = "version-clean",
          scenario = "version/reject-invalid-bump"
        }
