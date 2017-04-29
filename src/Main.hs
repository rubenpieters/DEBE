module Main where

import Control.Lens
import Control.Monad

import Data.List
import Data.Maybe


data InputExample = InputExample
                 { _pos1 :: Int
                 , _pos2 :: Int
                 , _inputText :: String
                 , _outputEx :: String
               } deriving (Show, Eq)

data SimpleExpr = ConstStr String
                | SubStr PosExpr PosExpr
                deriving (Show, Eq)

data ConcatenateExpr = Concatenate [SimpleExpr]
                     deriving (Show, Eq)

data LoopExpr = Loop (Int -> Dag)
              --deriving (Show, Eq)

data PosExpr = CPos Int
             | Pos String String Int
             deriving (Show, Eq)

isPrePost :: String -> String -> (String, String) -> Bool
isPrePost pre post (prefix, suffix) = isSuffixOf pre prefix && isPrefixOf post suffix

posIndex :: PosExpr -> String -> Maybe Int
posIndex (CPos i) str = mfilter (\_ -> i > 0 && i <= length str) (pure i)
posIndex (Pos pre post x) str = findIndices (isPrePost pre post) positions ^? element x
  where
    positions = zip (inits str) (tails str)

evalSimple :: SimpleExpr -> String -> Maybe String
evalSimple (ConstStr x) _ = pure x
evalSimple (SubStr posE1 posE2) str = pure slice <*> pos1 <*> pos2 <*> pure str
  where
   pos1 = posIndex posE1 str
   pos2 = posIndex posE2 str

evalConcat :: ConcatenateExpr -> String -> String
evalConcat (Concatenate l) str = join $ mapMaybe (`evalSimple` str) l

evalDag :: Dag -> String -> [String]
evalDag dag str = allExprEvals
  where
    allExprs = map snd $ allPaths dag
    allExprEvals = map (`evalConcat` str) allExprs

evalLoop :: LoopExpr -> String -> [String]
evalLoop (Loop dagF) str = crossJoined
  where
    dags = map (fmap snd . allPaths . dagF) [0..]
    evaledExprs = takeWhile (not . null . join) $ map (map (`evalConcat` str)) dags
    crossJoined = (map join . sequence) evaledExprs

allPaths :: Dag -> [(Int, ConcatenateExpr)]
allPaths dag = allPaths' (lastNode dag) dag (0, Concatenate [])

allPaths' :: Int -> Dag -> (Int, ConcatenateExpr) -> [(Int, ConcatenateExpr)]
allPaths' lim _ n@(src, _) | src == lim = [n]
allPaths' lim dag (src, Concatenate expr) = nextNodes' >>= allPaths' lim dag
  where
    nextNodes = filter (\(src', _, _) -> src == src') dag
    nextNodes' = do
      (_, nextTgt, nextExpr) <- nextNodes
      return (nextTgt, Concatenate $ expr ++ nextExpr)

type Node = (Int, Char)
type Edge = (Int, Int, [SimpleExpr])
type Dag = [Edge]

data CreationGraph = CreationGraph
                   { _nodes :: [Node]
                   , _edges :: Dag
                 } deriving (Show, Eq)

lastNode :: Dag -> Int
lastNode dag = maximum (map (\(_,x,_) -> x) dag)

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from) . drop from

graph :: CreationGraph -> Int -> Int -> String
graph (CreationGraph nodes _) from to =
  map snd $ slice from to nodes

example1 = InputExample 0 0 "Ruben Pieters 123" "RP123"

creationGraph :: InputExample -> CreationGraph
creationGraph ex@(InputExample p1 p2 input output) =
  CreationGraph nodes edges
    where
      nodes = zip [0..] output
      edges = [(i1, i2, compatibleSimpleExpr ex (slice i1 i2 output))
                | (i1, node1):rest <- tails nodes, (i2, node2) <- rest]

compatibleSimpleExpr :: InputExample -> String -> [SimpleExpr]
compatibleSimpleExpr (InputExample p1 p2 input output) str =
  [ConstStr str]
  where
    (before, (text, after)) = fmap (splitAt p2) (splitAt p1 input)

data VSA a = Leaf a
             | Union (VSA a) (VSA a)
             | Intersection (VSA a) (VSA a)
             deriving (Show, Eq)

testLoop :: Int -> Dag
testLoop i = [(0, 1, [SubStr (Pos "(" "" i) (Pos "" ")" i)]), (0, 1, [SubStr (Pos "(" "" i) (Pos "" ")" i)])]

main :: IO ()
main = do
  putStrLn "debe"
