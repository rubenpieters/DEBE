module Graph where

import Control.Lens
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe

import Token

data InputExample t = InputExample
                 { _pos1 :: Int
                 , _pos2 :: Int
                 , _inputText :: ParsedTknSeq t
                 , _outputEx :: String
               } deriving (Show, Eq)

data SimpleExpr t = ConstStr String
                | SubStr (PosExpr t) (PosExpr t)
                deriving (Show, Eq)

data ConcatenateExpr t = Concatenate [SimpleExpr t]
                     deriving (Show, Eq)

data LoopExpr t = Loop (Int -> Dag t)
              --deriving (Show, Eq)

data PosExpr t = CPos Int
             | Pos (QueryTknSeq t) (QueryTknSeq t) Int
             deriving (Show, Eq)

guarded :: (Alternative f) => a -> Bool -> f a
guarded a b = const a <$> guard b

isPrePost :: (Eq t) => QueryTknSeq t -> QueryTknSeq t -> ([t], [t]) -> Bool
isPrePost preTkns postTkns (prefix, suffix) =
  tknMatchPost preTkns prefix && tknMatchPre postTkns suffix

matchesPrePost :: (Eq t) => QueryTknSeq t -> QueryTknSeq t -> [t] -> [Int]
matchesPrePost pref post tkns = findIndices (isPrePost pref post) positions
  where positions = zip (inits tkns) (tails tkns)

posIndex :: (Eq t) => PosExpr t -> [t] -> Maybe Int
posIndex (CPos i) tkns | i >= 0 = guarded i (i <= length tkns)
posIndex (CPos i) tkns = guarded (length tkns + i) (-i <= length tkns)
posIndex (Pos pref post x) tkns | x >= 0 = foundPositions ^? element x
  where foundPositions = matchesPrePost pref post tkns
posIndex (Pos pref post x) tkns = foundPositions ^? element (length foundPositions + x)
  where foundPositions = matchesPrePost pref post tkns

evalSimple :: (Eq t) => SimpleExpr t -> ParsedTknSeq t -> Maybe String
evalSimple (ConstStr x) _ = pure x
evalSimple (SubStr posE1 posE2) parsed = fmap concat slicedTkns
  where
    tkns = map fst parsed
    strs = map snd parsed
    pos1 = posIndex posE1 tkns
    pos2 = posIndex posE2 tkns
    slicedTkns = slice <$> pos1 <*> pos2 <*> pure strs

intersectPos :: (Eq t) => PosExpr t -> PosExpr t -> Maybe (PosExpr t)
intersectPos (CPos k1) (CPos k2) = guarded (CPos k1) (k1 == k2)
intersectPos (Pos r1 r2 c) (Pos r1' r2' c') | (length r1 == length r1' && length r2 == length r2') =
  Pos <$> t1 <*> t2 <*> guarded c (c == c')
  where
    t1 = zipWithM tknIntersect r1 r1'
    t2 = zipWithM tknIntersect r2 r2'
intersectPos _ _ = Nothing

intersectSimple :: (Eq t) => SimpleExpr t -> SimpleExpr t -> Maybe (SimpleExpr t)
intersectSimple (ConstStr s1) (ConstStr s2) = guarded (ConstStr s1) (s1 == s2)
intersectSimple (SubStr p1 p2) (SubStr p1' p2') =
  SubStr <$> intersectPos p1 p1' <*> intersectPos p2 p2'
intersectSimple _ _ = Nothing

intersectDag :: (Eq t) => Dag t -> Dag t -> Dag t
intersectDag dag1 dag2 = simplified
  where
    xnodes = lastNode dag1 * lastNode dag2
    xdag = (,) <$> dag1 <*> dag2
    simplified = map (uncurry (intersectEdge xnodes)) xdag

-- remove all edges which cannot reach the end position
-- or cannot be reached from the starting position
trimDag :: Dag t -> Dag t
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

intersectEdge :: (Eq t) => Int -> Edge t -> Edge t -> Edge t
intersectEdge x (s, t, expr) (s', t', expr') =
  (s * x + s', t * x + t', catMaybes intersectedExpr)
    where
      xprodExpr = (,) <$> expr <*> expr'
      intersectedExpr = map (uncurry intersectSimple) xprodExpr

evalConcat :: (Eq t) => ConcatenateExpr t -> ParsedTknSeq t -> String
evalConcat (Concatenate l) str = join $ mapMaybe (`evalSimple` str) l

evalDag :: (Eq t) => Dag t -> ParsedTknSeq t -> [String]
evalDag dag str = allExprEvals
  where
    allExprs = map snd $ allPaths dag
    allExprEvals = map (`evalConcat` str) allExprs

evalLoop :: (Eq t) => LoopExpr t -> ParsedTknSeq t -> [String]
evalLoop (Loop dagF) str = crossJoined
  where
    dags = map (fmap snd . allPaths . dagF) [0..]
    evaledExprs = takeWhile (not . null . join) $ map (map (`evalConcat` str)) dags
    -- probably should filter all "" away before doing the sequence
    crossJoined = (map join . sequence) evaledExprs

allPaths :: (Eq t) => Dag t -> [(Int, ConcatenateExpr t)]
allPaths dag = allPaths' (lastNode dag) dag (0, Concatenate [])

allPaths' :: (Eq t) => Int -> Dag t -> (Int, ConcatenateExpr t) -> [(Int, ConcatenateExpr t)]
allPaths' lim _ n@(src, _) | src == lim = [n]
allPaths' lim dag (src, Concatenate expr) = nextNodes' >>= allPaths' lim dag
  where
    nextNodes = filter (\(src', _, _) -> src == src') dag
    nextNodes' = do
      (_, nextTgt, nextExprs) <- nextNodes
      nextExpr <- nextExprs
      return (nextTgt, Concatenate $ expr ++ [nextExpr])

type Node t = (Int, Char)
type Edge t = (Int, Int, [SimpleExpr t])
type Dag t = [Edge t]

data CreationGraph t = CreationGraph
                   { _nodes :: [Node t]
                   , _edges :: Dag t
                 } deriving (Show, Eq)

lastNode :: Dag t -> Int
lastNode dag = maximum (map (\(_,x,_) -> x) dag)

transitions :: Dag t -> [(Int, Int, Int)]
transitions = map (\(a,b,c) -> (a,b,length c))

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from) . drop from

creationGraph :: (Eq t, Generalize t) => InputExample t -> CreationGraph t
creationGraph ex@(InputExample p1 p2 input output) =
  CreationGraph nodes edges
    where
      nodes = (0, ' ') : (zip [1..] output)
      edges = [(i1, i2, generateStr ex (slice i1 i2 output))
                | (i1, node1):rest <- tails nodes, (i2, node2) <- rest]

generateStr :: (Eq t, Generalize t) => InputExample t -> String -> [SimpleExpr t]
generateStr (InputExample p1 p2 input output) str =
  ConstStr str : generateSubstring text str
  where
    (before, (text, after)) = fmap (splitAt (p2 - p1)) (splitAt p1 input)

generateSubstring :: (Eq t, Generalize t) => ParsedTknSeq t -> String -> [SimpleExpr t]
generateSubstring input output = SubStr <$> y1 <*> y2
  where
    y1 = allMatches input output >>= generatePosition input
    y2 = allMatches input output >>= generatePosition input . (+ length output)

-- the starting positions of all matches of output in input
allMatches :: (Eq t) => ParsedTknSeq t -> String -> [Int]
allMatches input output = map fst $ filter (snd . fmap (isPrefixOf output . concat)) indexedStrs
  where
    inputStrs = map snd input
    indexedStrs = zip [0..] (tails inputStrs)

xthMatchIn :: (Eq t) => [t] -> QueryTknSeq t -> QueryTknSeq t -> Int -> Maybe Int
xthMatchIn input srchPre srchPost posInInput =
  elemIndex
    (posInInput)
    (matchesPrePost srchPre srchPost input)

-- returns (cth match, total matches)
matchesInS :: (Eq t) => [t] -> QueryTknSeq t -> QueryTknSeq t -> Int -> (Maybe Int, Int)
matchesInS tkns pr ps k' = (xthMatchIn tkns pr ps k', length $ matchesPrePost pr ps tkns)

generatePosition :: (Eq t, Generalize t) => ParsedTknSeq t -> Int -> [PosExpr t]
generatePosition s k = concatMap createPos combinations
  where
    tkns = map fst s
    (preTxt, postTxt) = splitAt k (map (generalizeTkn . fst) s)
    preTokens = take 3 $ (reverse . tails) preTxt
    postTokens = take 3 $ (map reverse . reverse . tails . reverse) postTxt
    combinations = filter (\(a, b) -> a /= [] || b /= []) $ (,) <$> preTokens <*> postTokens
    positions (Just c, c') = [c, -(c' - c)]
    positions (Nothing, _) = []
    createPos (a,b) = map (Pos a b) (positions $ matchesInS tkns a b k)

data VSA a = Leaf a
             | Union (VSA a) (VSA a)
             | Intersection (VSA a) (VSA a)
             deriving (Show, Eq)
