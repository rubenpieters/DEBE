module TokenSpec (main, spec) where

import Token

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "a" $ do
    it "should a" $ do
      "a" `shouldBe` "a"
