module Graph where

import Control.Lens
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe

import Token

data InputExample = InputExample
                 { _pos1 :: Int
                 , _pos2 :: Int
                 , _inputText :: TknSeq
                 , _outputEx :: TknSeq
               } deriving (Show, Eq)

data SimpleExpr = ConstStr TknSeq
                | SubStr PosExpr PosExpr
                deriving (Show, Eq)

data ConcatenateExpr = Concatenate [SimpleExpr]
                     deriving (Show, Eq)

data LoopExpr = Loop (Int -> Dag)
              --deriving (Show, Eq)

data PosExpr = CPos Int
             | Pos TknSeq TknSeq Int
             deriving (Show, Eq)

guarded :: (Alternative f) => a -> Bool -> f a
guarded a b = const a <$> guard b

isPrePost :: TknSeq -> TknSeq -> (TknSeq, TknSeq) -> Bool
isPrePost [] [] _ = True
isPrePost [] post (_, suffix) = post `isPrefixOf` suffix
isPrePost pref [] (prefix, _) = pref `isSuffixOf` prefix
isPrePost pref post (prefix, suffix) = isSuffixOf pref prefix && isPrefixOf post suffix

posIndex :: PosExpr -> TknSeq -> Maybe Int
posIndex (CPos i) str = guarded i (i > 0 && i <= length str)
posIndex (Pos pref post x) str = findIndices (isPrePost pref post) positions ^? element x
  where
    positions = zip (inits str) (tails str)

evalSimple :: SimpleExpr -> TknSeq -> Maybe TknSeq
evalSimple (ConstStr x) _ = pure x
evalSimple (SubStr posE1 posE2) str = slice <$> pos1 <*> pos2 <*> pure str
  where
    pos1 = posIndex posE1 str
    pos2 = posIndex posE2 str

intersectPos :: PosExpr -> PosExpr -> Maybe PosExpr
intersectPos (CPos k1) (CPos k2) = guarded (CPos k1) (k1 == k2)
intersectPos (Pos r1 r2 c) (Pos r1' r2' c') =
  Pos <$> t1 <*> t2 <*> guarded c (c == c')
  where
    t1 = zipWithM tknIntersect r1 r1'
    t2 = zipWithM tknIntersect r2 r2'
intersectPos _ _ = Nothing

intersectSimple :: SimpleExpr -> SimpleExpr -> Maybe SimpleExpr
intersectSimple (ConstStr s1) (ConstStr s2) = guarded (ConstStr s1) (s1 == s2)
intersectSimple (SubStr p1 p2) (SubStr p1' p2') =
  SubStr <$> intersectPos p1 p1' <*> intersectPos p2 p2'
intersectSimple _ _ = Nothing

intersectDag :: Dag -> Dag -> Dag
intersectDag dag1 dag2 = simplified
  where
    xnodes = lastNode dag1 * lastNode dag2
    xdag = (,) <$> dag1 <*> dag2
    simplified = map (uncurry (intersectEdge xnodes)) xdag

-- remove all edges which cannot reach the end position
-- or cannot be reached from the starting position
trimDag :: Dag -> Dag
trimDag dag = filter (\(a,b,_) -> elem (a,b) (reachableNodesStable nodesDag)) dag
  where
    nodesDag = map (\(a,b,_) -> (a,b)) dag
    reachableNodesStable = converge (==) . iterate (reachableNodes (lastNode dag))

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y     = y
  | otherwise = converge p ys

reachableNodes :: Int -> [(Int, Int)] -> [(Int, Int)]
reachableNodes lastN allNodes = filter
  (\(a,b) -> ((a == 0) || elem b (map fst allNodes)) && (elem a (map snd allNodes) || (b == lastN)))
  allNodes

intersectEdge :: Int -> Edge -> Edge -> Edge
intersectEdge x (s, t, expr) (s', t', expr') =
  (s * x + s', t * x + t', catMaybes intersectedExpr)
    where
      xprodExpr = (,) <$> expr <*> expr'
      intersectedExpr = map (uncurry intersectSimple) xprodExpr

evalConcat :: ConcatenateExpr -> TknSeq -> TknSeq
evalConcat (Concatenate l) str = join $ mapMaybe (`evalSimple` str) l

evalDag :: Dag -> TknSeq -> [TknSeq]
evalDag dag str = allExprEvals
  where
    allExprs = map snd $ allPaths dag
    allExprEvals = map (`evalConcat` str) allExprs

evalLoop :: LoopExpr -> TknSeq -> [TknSeq]
evalLoop (Loop dagF) str = crossJoined
  where
    dags = map (fmap snd . allPaths . dagF) [0..]
    evaledExprs = takeWhile (not . null . join) $ map (map (`evalConcat` str)) dags
    -- probably should filter all "" away before doing the sequence
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

type Node = (Int, Token)
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

graph :: CreationGraph -> Int -> Int -> TknSeq
graph (CreationGraph nodes _) from to =
  map snd $ slice from to nodes

creationGraph :: InputExample -> CreationGraph
creationGraph ex@(InputExample p1 p2 input output) =
  CreationGraph nodes edges
    where
      nodes = zip [0..] output
      edges = [(i1, i2, generateStr ex (slice i1 i2 output))
                | (i1, node1):rest <- tails nodes, (i2, node2) <- rest]

generateStr :: InputExample -> TknSeq -> [SimpleExpr]
generateStr (InputExample p1 p2 input output) str =
  [ConstStr str] ++ generateSubstring text str
  where
    (before, (text, after)) = fmap (splitAt (p2 - p1)) (splitAt p1 input)

generateSubstring :: TknSeq -> TknSeq -> [SimpleExpr]
generateSubstring input output = SubStr <$> y1 <*> y2
  where
    y1 = allMatches input output >>= generatePosition input
    y2 = allMatches input output >>= generatePosition input . (+ length output)

-- the starting positions of all matches of output in input
allMatches :: (Eq a) => [a] -> [a] -> [Int]
allMatches input output = map fst $ filter (snd . fmap (isPrefixOf output)) (zip [0..] (tails input))

xthMatchIn :: TknSeq -> TknSeq -> TknSeq -> Int -> Maybe Int
xthMatchIn input srchPre srchPost posInInput =
  elemIndex
    (posInInput - length srchPre)
    (allMatches input (srchPre ++ srchPost))

generatePosition :: TknSeq -> Int -> [PosExpr]
generatePosition s k = concatMap createPos combinations
  where
    (preTxt, postTxt) = splitAt k s
    preTokens = take 3 $ (reverse . tknTails) preTxt
    postTokens = take 3 $ (map reverse . reverse . tknTails . reverse) postTxt
    combinations = filter (\(a, b) -> a /= emptyToken || b /= emptyToken) $ (,) <$> preTokens <*> postTokens
    matchesInS pr ps k' = (xthMatchIn s pr ps k', length $ allMatches s (pr ++ ps)) -- (cth match, total matches)
    positions (Just c, c') = [c, -(c' - c)]
    positions (Nothing, _) = []
    createPos (a,b) = map (Pos a b) (positions $ matchesInS a b k)

data VSA a = Leaf a
             | Union (VSA a) (VSA a)
             | Intersection (VSA a) (VSA a)
             deriving (Show, Eq)
