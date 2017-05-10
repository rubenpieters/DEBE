module TokenSpec (main, spec) where

import Token

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tknMatchPre" $ do
    it "should pass simple case" $ do
      tknMatchPre
        [litToken AZToken, litToken NumToken, litToken AZToken]
        [AZToken, NumToken, AZToken, SpecialToken, AZToken, NumToken]
      `shouldBe` True
    it "should pass simple negative case" $ do
      tknMatchPre
        [litToken AZToken, litToken NumToken, litToken AZToken]
        [NumToken, AZToken, SpecialToken, AZToken, NumToken, AZToken]
      `shouldBe` False
    it "should pass case with empty query list" $ do
      tknMatchPre
        []
        [AZToken, NumToken, AZToken, SpecialToken, AZToken, NumToken]
      `shouldBe` True
    it "should pass case with empty parsed tokens list" $ do
      tknMatchPre
        [litToken AZToken, litToken NumToken, litToken AZToken]
        []
      `shouldBe` False
    it "should pass case with query tokens larger than parsed tokens" $ do
      tknMatchPre
        [litToken NumToken, litToken NumToken]
        [NumToken]
      `shouldBe` False
  describe "tknMatchPost" $ do
    it "should pass simple case" $ do
      tknMatchPost
        [litToken AZToken, litToken NumToken, litToken AZToken]
        [NumToken, AZToken, SpecialToken, AZToken, NumToken, AZToken]
      `shouldBe` True
    it "should pass simple negative case" $ do
      tknMatchPost
        [litToken AZToken, litToken NumToken, litToken AZToken]
        [AZToken, NumToken, AZToken, SpecialToken, AZToken, NumToken]
      `shouldBe` False
    it "should pass case with query tokens larger than parsed tokens" $ do
      tknMatchPre
        [litToken NumToken, litToken NumToken]
        [NumToken]
      `shouldBe` False
