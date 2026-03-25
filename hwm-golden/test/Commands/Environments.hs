module Commands.Environments (testEnvironments) where

import HWM.Golden (Golden (..), goldenFailTest, goldenTest)
import Test.Hspec (Spec, describe, it)

testEnvironments :: Spec
testEnvironments = describe "environments" $ do
  it "removes a non-default environment" $
    goldenTest
      Golden
        { cmd = "environments remove ci",
          project = "simple",
          scenario = "environments/remove/non-default"
        }

  it "removes current default environment with explicit migration" $
    goldenTest
      Golden
        { cmd = "environments remove default --set-default ci",
          project = "simple",
          scenario = "environments/remove/default-migrate"
        }

  it "rejects removing current default without --set-default" $
    goldenFailTest
      Golden
        { cmd = "environments remove default",
          project = "simple",
          scenario = "environments/remove/reject-default-without-migration"
        }

  it "rejects removing non-existing environment" $
    goldenFailTest
      Golden
        { cmd = "environments remove ghost",
          project = "simple",
          scenario = "environments/remove/reject-non-existing"
        }
