{-# LANGUAGE MultiParamTypeClasses #-}

module Token
  ( Token(..)
  , TknSeq
  , tknTails
  , tknParseString
  , tknMatch
  , tknIntersect
  ) where

import Data.Char
import Data.List

type TknSeq = [Token]

data Token = AZToken
           | NumToken
           | SpecialToken
           | AllToken
           | EmptyToken -- reconsider using [] as EmptyToken?
           deriving (Show, Eq)

-- like the Data.List tails function
-- , but instead of empty list returns [EmptyToken] at the end
tknTails :: [Token] -> [[Token]]
tknTails l = init (tails l) ++ [[EmptyToken]]

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
