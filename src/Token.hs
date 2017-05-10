{-# LANGUAGE MultiParamTypeClasses #-}

module Token
  ( SimpleToken(..)
  , ParsedTknSeq
  , emptyToken
  --, tknTails
  , tknParseString
  , tknMatch
  , tknIntersect
  , ParsedToken(..)
  , ParsedTknSeq(..)
  , QueryTknSeq(..)
  , QueryToken(..)
  , litToken
  , tknMatchPre
  , tknMatchPost
  ) where

import Data.Char
import Data.List

--emptyToken :: ParsedTknSeq t
--emptyToken = []

data SimpleToken = AZToken
           | NumToken
           | SpecialToken
           deriving (Show, Eq)

type ParsedToken t = (t, String)
type ParsedTknSeq t = [ParsedToken t]

data QueryToken t = AnyOfToken [t]
                  | AnyToken
                  deriving (Show, Eq)

type QueryTknSeq t = [QueryToken t]

litToken :: t -> QueryToken t
litToken t = AnyOfToken [t]

emptyToken :: QueryToken t
emptyToken = AnyOfToken []

-- like the Data.List tails function
-- , but instead of empty list returns [EmptyToken] at the end
--tknTails :: [Token] -> [[Token]]
--tknTails l = init (tails l) ++ [emptyToken]

tknParseChar :: Char -> ParsedToken SimpleToken
tknParseChar x | isAlpha x = (AZToken, [x])
tknParseChar x | isDigit x = (NumToken, [x])
tknParseChar x = (SpecialToken, [x])

tknParseString :: String -> [ParsedToken SimpleToken]
tknParseString = map tknParseChar

tknMatch :: (Eq t) => t -> QueryToken t -> Bool
tknMatch x (AnyOfToken l) | x `elem` l = True
tknMatch _ AnyToken = True
tknMatch _ _ = False

tknMatchPre :: (Eq t) => QueryTknSeq t -> [t] -> Bool
tknMatchPre [] _ = True
tknMatchPre _ [] = False
tknMatchPre qTkns tkns | length qTkns > length tkns = False
tknMatchPre qTkns tkns = all (uncurry tknMatch) $ zip tkns qTkns

tknMatchPost :: (Eq t) => QueryTknSeq t -> [t] -> Bool
tknMatchPost a b = tknMatchPre (reverse a) (reverse b)

tknIntersect :: (Eq t) => QueryToken t -> QueryToken t -> Maybe (QueryToken t)
tknIntersect AnyToken tkn = Just tkn
tknIntersect tkn AnyToken = Just tkn
tknIntersect (AnyOfToken l1) (AnyOfToken l2) =
  if null inters
    then Nothing
    else Just (AnyOfToken inters)
  where inters = l1 `intersect` l2
