module Commands.Sync (spec) where

import Test.Hspec
import Utils.Golden (Golden (..), goldenTest)

spec :: Spec
spec = describe "hwm sync (golden tests)" $ do
  it "syncs a simple workspace correctly" $ goldenTest Golden {cmd = "sync", scenario = "sync/simple"}
  it "syncs a cabal-only workspace correctly" $ goldenTest Golden {cmd = "sync", scenario = "sync/cabal-only"}
