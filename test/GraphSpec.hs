module GraphSpec (main, spec) where

import Control.Monad

import Token
import Graph

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPrePost" $ do
    it "should pass simple case" $ do
      isPrePost [litToken NumToken, litToken AZToken] [litToken NumToken]
        ([SpecialToken, NumToken, AZToken], [NumToken, SpecialToken])
      `shouldBe` True
    it "should pass case with empty token" $ do
      isPrePost [] [litToken NumToken, litToken NumToken]
        ([], [NumToken, NumToken, AZToken])
      `shouldBe` True
    it "should pass where query is larger than rest of tokens" $ do
      isPrePost [] [litToken NumToken, litToken NumToken]
        ([AZToken, NumToken], [NumToken])
      `shouldBe` False
  describe "matchesPrePost" $ do
    it "should pass simple case" $ do
      let pref =  []
      let post =  [litToken NumToken, litToken NumToken]
      let tkns = [AZToken, AZToken, NumToken, NumToken]
      matchesPrePost pref post tkns `shouldBe` [2]
  describe "posIndex" $ do
    it "should pass simple case" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [litToken AZToken] [litToken NumToken] 0) input `shouldBe` Just 1
    it "should pass case with multiple tokens" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [litToken AZToken, litToken NumToken] [litToken AZToken] 0) input `shouldBe` Just 2
    it "should pass case with empty token" $ do
      let input = [AZToken, NumToken, AZToken]
      posIndex (Pos [] [litToken NumToken] 0) input `shouldBe` Just 1
    it "should pass case with negative CPos" $ do
      let str = [AZToken, SpecialToken, AZToken, SpecialToken, AZToken, SpecialToken]
      posIndex (Pos [litToken AZToken] [litToken SpecialToken] (-1)) str `shouldBe` Just 5
    it "should pass case with negative CPos 2" $ do
      let str = [AZToken, AZToken, NumToken, NumToken]
      posIndex (Pos [] [litToken NumToken, litToken NumToken] (-1)) str `shouldBe` Just 2
  describe "evalSimple" $ do
    it "should pass SubStr case" $ do
      let subStrExpr = SubStr (Pos [] [litToken AZToken] 0) (Pos [litToken NumToken] [litToken NumToken] (-1))
      let parsedTkns = [(AZToken, "a"), (AZToken, "b"), (NumToken, "1"), (NumToken, "2")]
      evalSimple subStrExpr parsedTkns `shouldBe` Just "ab1"
  describe "evalConcat" $ do
    it "should pass SubStr case" $ do
      let subStrExpr = SubStr (Pos [] [litToken AZToken] 0) (Pos [litToken NumToken] [litToken NumToken] (-1))
      let constExpr = ConstStr "cd2"
      let concatExpr = Concatenate [subStrExpr, constExpr]
      let parsedTkns = [(AZToken, "a"), (AZToken, "b"), (NumToken, "1"), (NumToken, "2")]
      evalConcat concatExpr parsedTkns `shouldBe` "ab1cd2"
  describe "generatePosition" $ do
    it "should pass simple case" $ do
      let parsedTkns = [(AZToken, "a"), (AZToken, "b"), (NumToken, "1"), (NumToken, "2")]
      let positions = generatePosition parsedTkns 2
      forM_ positions (putStrLn . show)
      map (`posIndex` map fst parsedTkns) positions `shouldBe` replicate (length positions) (Just 2)
      --length positions `shouldSatisfy` (== 9)
