{-
https://www.reddit.com/r/dailyprogrammer/comments/3z1cxs/20160101_challenge_247_hard_zombies_on_the/
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Data.Array
import           Data.Attoparsec.Text
import           Data.Foldable
import           Data.Function
import           Data.Heap            (Entry (..), Heap)
import qualified Data.Heap            as Heap
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.List
import           Data.Maybe
import           Data.Sequence        (Seq, ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import qualified Data.Sequence        as Seq
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Time.Clock
import           Data.Tuple

type Vertex = Int
type Weight = Int
type Edge = (Vertex, Vertex, Weight)
type Graph = Array (Vertex,Vertex) (Maybe Weight)

data Step
  = Move Vertex Vertex Weight
  | Blast Vertex Vertex

instance Show Step where
  show (Move a b _) = show a ++ " to " ++ show b
  show (Blast a b) = show a ++ " *BLAST* to " ++ show b

travel :: Step -> Weight
travel (Move _ _ w) = w
travel _            = 0

main :: IO ()
main = do
  start <- getCurrentTime
  let solve = maybe "No solution" results . challenge
      noParse = T.pack . ("Parse Error: "++)
  TIO.interact (either noParse solve . parseOnly edgesP)
  end <- getCurrentTime
  putStrLn $ "Time: " ++ show (end `diffUTCTime` start)

results :: [Step] -> Text
results steps = T.pack $ unlines [stepsMsg, totalMsg] where
  stepsMsg = intercalate ", " (map show steps)
  total = sum (map travel steps)
  totalMsg = "Reached Last Chance encountering " ++ show total ++ " zombies"

challenge :: [Edge] -> Maybe [Step]
challenge edges = do
  let dim@(start, end) = (0, maximum [max a b | (a,b,_) <- edges])
      dim' = ((start,start),(2*end,2*end))
      edges' = concatMap (bzfg dim) edges
      graph = accumArray (const id) Nothing dim' edges'
  path <- shortestPath start end graph
  let steps = zip path (drop 1 path)
  mapM (toStep graph dim) steps


toStep :: Graph -> (Vertex,Vertex) -> (Vertex,Vertex) -> Maybe Step
toStep graph (start,end) (a,b)
  | isBlast   = Just (Blast x y)
  | otherwise = Move x y <$> graph ! (a,b)
  where vertCount = rangeSize (start,end)
        [x,y] = map (`mod` vertCount) [a,b]
        isBlast = abs (a - b) > vertCount || (a < end && b == end)

bzfg :: (Vertex,Vertex) -> Edge -> [((Vertex,Vertex), Maybe Weight)]
bzfg dim@(_,end) (x,y,w)
  | b /= end  = [ (( a,  b ), Just w ), (( b , a ), Just w )  --No blast ab
                , (( a', b'), Just 0 ), (( b', a'), Just 0 )  --Was blast ab
                , (( a , b'), Just 0 )]                       --The blast
  | otherwise = [ (( a , b ), Just 0 ), (( b , a ), Just 0 )  --if not used, last move is blast
                , (( a', b ), Just w )]                       --Connect blasts to orig
  where (a, b) = (min x y, max x y) --sorted to simplify when b == end
        (a',b') = (a + vertCount, b + vertCount)
        vertCount = rangeSize dim

edgesP :: Parser [Edge]
edgesP = edgeP `sepBy1` string ", " <* optional skipSpace <* endOfInput

edgeP :: Parser Edge
edgeP = (,,) <$ char '(' <*> decimal <* string ", " <*> decimal <* string ", " <*> decimal <* char ')'

transposeG :: Graph -> Graph
transposeG g = ixmap (bounds g) swap g

neighbors :: Vertex -> Graph -> [(Vertex,Weight)]
neighbors a g = catMaybes [ (b,) <$> w
                          | let ((low,_),(hi,_)) = bounds g
                          , b <- range (low, hi)
                          , let w = g ! (a,b) ]

data SearchState = SearchState
    { frontier :: Heap (Entry Weight (Seq Vertex))
    , known    :: IntMap (Seq Vertex)
    , cons     :: Vertex -> Seq Vertex -> Seq Vertex
    , snoc     :: Seq Vertex -> Maybe (Vertex, Seq Vertex)
    , graph    :: Graph }

shortestPath :: Vertex -> Vertex -> Graph -> Maybe [Vertex]
shortestPath a b g = go sa0 sb0
  where
    sa0 = SearchState
      { frontier = Heap.singleton (Entry 0 (Seq.singleton a))
      , known = IntMap.empty
      , cons = flip (|>)
      , snoc = snocRight
      , graph = g }

    sb0 = SearchState
      { frontier = Heap.singleton (Entry 0 (Seq.singleton b))
      , known = IntMap.empty
      , cons = (<|)
      , snoc = snocLeft
      , graph = transposeG g }

    go :: SearchState -> SearchState -> Maybe [Vertex]
    go sa sb = toList <$> connect sa sb <|> do
        sa' <- expand sa
        sb' <- expand sb
        go sa' sb'

    connect :: SearchState -> SearchState -> Maybe (Seq Vertex)
    connect sa sb = join $ root $ IntMap.intersectionWith merge ka kb where
        [ka,kb] = known <$> [sa,sb]
        merge xs ys = do
            (_, ys') <- snoc sb ys
            return (xs >< ys')

    expand :: SearchState -> Maybe SearchState
    expand s@SearchState{..} = do
      (Entry _ path, old) <- Heap.viewMin frontier
      (x, _) <- snoc path
      let new = [Entry w (cons v path) | (v,w) <- neighbors x graph, IntMap.notMember v known]
          withDup = Heap.union old (Heap.fromList new)
      return s
        { frontier = nubPayloadBy ((==) `on` fmap fst . snoc) withDup
        , known    = IntMap.insert x path known  }

root :: IntMap a -> Maybe a
root = fmap (snd . fst) . uncons . IntMap.toList

nubPayloadBy :: (Ord a) => (b -> b -> Bool) -> Heap (Entry a b) -> Heap (Entry a b)
nubPayloadBy f = Heap.map Heap.minimum . Heap.groupBy (f `on` Heap.payload)

snocRight, snocLeft :: Seq a -> Maybe (a, Seq a)
snocRight s | (xs :> x) <- viewr s = Just (x, xs)
            | otherwise            = Nothing
snocLeft s  | (x :< xs) <- viewl s = Just (x, xs)
            | otherwise            = Nothing
