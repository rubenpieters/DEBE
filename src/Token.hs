{-# LANGUAGE MultiParamTypeClasses #-}

module Token
  ( Token(..)
  , TknSeq
  , tknParseString
  , tknMatch
  , tknIntersect
  ) where

import Data.Char

type TknSeq = [Token]

data Token = AZToken
           | NumToken
           | SpecialToken
           | AllToken
           | EmptyToken
           deriving (Show, Eq)

tknParseChar :: Char -> Token
tknParseChar x | isAlpha x = AZToken
tknParseChar x | isDigit x = NumToken
tknParseChar _ = SpecialToken

tknParseString :: String -> [Token]
tknParseString = map tknParseChar

tknMatch :: Token -> Token -> Bool
tknMatch AZToken AZToken = True
tknMatch NumToken NumToken = True
tknMatch SpecialToken SpecialToken = True
tknMatch AllToken _ = True
tknMatch EmptyToken _ = False
tknMatch _ _ = False

tknIntersect :: Token -> Token -> Maybe Token
tknIntersect AllToken x = Just x
tknIntersect x AllToken = Just x
tknIntersect a b | a == b = Just a
tknIntersect _ _ = Nothing
