module NNU.App.TwitterBotSpec
  ( spec
  ) where

import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "dummy" $ do
    it "obvious" $ do
      True `shouldBe` True
