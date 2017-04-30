module Main where

import Token
import Graph

e = EmptyToken
s = SpecialToken
az = AZToken
n = NumToken
all' = AllToken

testLoop :: Int -> Dag
testLoop i = [(0, 1, [SubStr (Pos [s] [e] i) (Pos [e] [s] i)])]

example1 = InputExample 0 0 (tknParseString "Ruben Pieters 123") (tknParseString "RP123")

dag1 :: Dag
dag1 = [ (0, 1, [SubStr (Pos [s] [e] 0) (Pos [e] [s] 0)])
       , (0, 2, [SubStr (Pos [s] [e] 0) (Pos [e] [s] 0)])
       , (1, 2, [SubStr (Pos [s] [e] 0) (Pos [e] [s] 0)])
       ]

dag2 :: Dag
dag2 = [ (0, 1, [SubStr (Pos [all'] [e] 0) (Pos [e] [all'] 0)])
       , (0, 2, [SubStr (Pos [all'] [e] 0) (Pos [e] [all'] 0)])
       , (1, 2, [SubStr (Pos [all'] [e] 0) (Pos [e] [all'] 0)])
       ]

main :: IO ()
main = do
  putStrLn "debe"
