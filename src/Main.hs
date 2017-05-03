module Main where

import Token
import Graph

e = emptyToken
s = SpecialToken
az = AZToken
n = NumToken
all' = AllToken

testLoop :: Int -> Dag
testLoop i = [(0, 1, [SubStr (Pos [s] e i) (Pos e [s] i)])]

exampleRP = InputExample 0 0 (tknParseString "Ruben Pieters 123") (tknParseString "RP123")

dag1 :: Dag
dag1 = [ (0, 1, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       --, (0, 2, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       --, (1, 2, [SubStr (Pos [s] e 0) (Pos e [s] 0)])
       ]

dag2 :: Dag
dag2 = [ (0, 1, [SubStr (Pos [all'] e 0) (Pos e [all'] 0)])
       , (0, 2, [SubStr (Pos [all'] e 0) (Pos e [all'] 0)])
       , (1, 2, [SubStr (Pos [all'] e 0) (Pos e [all'] 0)])
       ]

example1 = InputExample 0 5 (tknParseString "0-1-2") (tknParseString "012")
str1 = creationGraph example1
example2 = InputExample 0 5 (tknParseString "0/1/2") (tknParseString "012")
str2 = creationGraph example2

inters = intersectDag (_edges str1) (_edges str2)

main :: IO ()
main = do
  putStrLn "debe"
