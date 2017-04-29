module Main where

import Data.List

data InputExample = InputExample
                 { _pos1 :: Int
                 , _pos2 :: Int
                 , _inputText :: String
                 , _outputEx :: String
               } deriving (Show, Eq)

data SimpleExpr = Const String
                | SubStr PosExpr PosExpr
                | Loop Dag
                deriving (Show, Eq)

data PosExpr = CPos Int
             | Pos String String Int
             deriving (Show, Eq)

type Node = (Int, Char)
type Edge = (Int, Int, [SimpleExpr])
type Dag = [Edge]

data CreationGraph = CreationGraph
                   { _nodes :: [Node]
                   , _edges :: Dag
                 } deriving (Show, Eq)

graph :: CreationGraph -> Int -> Int -> String
graph (CreationGraph nodes _) from to =
  map snd $ take (to-from) . drop from $ nodes

example1 = InputExample 0 0 "Ruben Pieters 123" "RP123"

creationGraph :: InputExample -> CreationGraph
creationGraph (InputExample p1 p2 input output) =
  CreationGraph nodes edges
    where
      nodes = zip [0..] output
      edges = [(i1, i2, [])
                | (i1, node1):rest <- tails nodes, (i2, node2) <- rest]

compatibleSimpleExpr :: InputExample -> String -> [SimpleExpr]
compatibleSimpleExpr (InputExample p1 p2 input output) str =
  [Const str]
  where
    (before, (text, after)) = fmap (splitAt p2) (splitAt p1 input)

data VSA a = Leaf a
             | Union (VSA a) (VSA a)
             | Intersection (VSA a) (VSA a)
             deriving (Show, Eq)



main :: IO ()
main = do
  putStrLn "debe"
