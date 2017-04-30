module GraphSpec (main, spec) where

import Token
import Graph

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "posIndex" $ do
    it "should pass simple case" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [AZToken] [NumToken] 0) input `shouldBe` Just 1
    it "should pass case with multiple tokens" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [AZToken, NumToken] [AZToken] 0) input `shouldBe` Just 2
    it "should pass case with empty token" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [EmptyToken] [NumToken] 0) input `shouldBe` Just 1
