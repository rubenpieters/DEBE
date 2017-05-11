module Main where

import Token
import Graph

e = []
s = litToken SpecialToken
az = litToken AZToken
n = litToken NumToken
all' = AnyToken

{-
testLoop :: Int -> Dag
testLoop i = [(0, 1, [SubStr (Pos [s] e i) (Pos e [s] i)])]

exampleRP = InputExample 0 0 (tknParseString "Ruben Pieters 123") (tknParseString "RP123")

-}
dag1 :: Dag SimpleToken
dag1 = [ (0, 1, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       --, (0, 2, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       --, (1, 2, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       ]

dag2 :: Dag SimpleToken
dag2 = [ (0, 1, [SubStr (Pos [all'] e 0) (Pos e [all'] 0)])
       , (0, 2, [SubStr (Pos [all'] e 0) (Pos e [all'] 0), SubStr (Pos [s] e 0) (Pos e [s] 0)])
       , (1, 2, [SubStr (Pos [all'] e 0) (Pos e [all'] 0)])
       ]

dag3 :: Dag SimpleToken
dag3 = [ (0, 1, [SubStr (Pos [all'] e 0) (Pos e [all'] 1)])
       , (0, 2, [SubStr (Pos [all'] e 0) (Pos e [all'] 2)])
       , (0, 3, [SubStr (Pos [all'] e 0) (Pos e [all'] 3)])
       , (1, 2, [SubStr (Pos [all'] e 1) (Pos e [all'] 2)])
       , (1, 3, [SubStr (Pos [all'] e 1) (Pos e [all'] 3)])
       , (2, 3, [SubStr (Pos [all'] e 2) (Pos e [all'] 3)])
       ]

example1 = InputExample 0 5 (tknParseString "0-1-2") "012"
str1 = creationGraph example1
example2 = InputExample 0 5 (tknParseString "3-4-5") "345"
str2 = creationGraph example2

inters = intersectDag (_edges str1) (_edges str2)

example1' = InputExample 0 4 (tknParseString "(ab)") "ab"
str1' = creationGraph example1'
example2' = InputExample 0 4 (tknParseString "-cd-") "cd"
str2' = creationGraph example2'

inters' = intersectDag (_edges str1') (_edges str2')

x = trimDag inters'
y = map snd (allPaths x)
z = map (`evalConcat` tknParseString "(ab)") y

main :: IO ()
main = do
  putStrLn "debe"
