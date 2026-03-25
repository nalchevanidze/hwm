module Commands.Run (testRun) where

import HWM.Golden (Golden (..), goldenTest)
import Test.Hspec (Spec, describe, it)

testRun :: Spec
testRun = describe "run" $ do
  it "forwards args after -- into the script command (implicit script routing)" $
    goldenTest
      Golden
        { cmd = "lint -- --fix",
          project = "simple",
          scenario = "run/forward-args"
        }

  it "forwards args after -- with explicit run command" $
    goldenTest
      Golden
        { cmd = "run lint -- --fix",
          project = "simple",
          scenario = "run/forward-args-explicit"
        }

  it "keeps spaced args intact when forwarding (e.g. 'a b' stays one argument)" $
    goldenTest
      Golden
        { cmd = "run argc -- 'a b' c",
          project = "simple",
          scenario = "run/forward-quoted-args"
        }

  it "forwards single quotes safely (e.g. it\'s stays intact)" $
    goldenTest
      Golden
        { cmd = "run arg1 -- \"it's ok\"",
          project = "simple",
          scenario = "run/forward-single-quote"
        }
