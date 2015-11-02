{-
https://www.reddit.com/r/dailyprogrammer/comments/3qtr01/20151030_challenge_238_hard_searching_a_dungeon/
-}

import Control.Monad
import Data.Function
import Data.Graph
import Data.Tree
import Data.List
import Data.Ord
import Data.Array
import Control.Monad.State
import qualified Data.Set as Set

type Point = (Int,Int,Int)

px, py, pz :: Point -> Int
px (x,_,_) = x
py (_,y,_) = y
pz (_,_,z) = z

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating = on (==)

readDung :: String -> [(Point,Char)]
readDung input =
  [ ((x,y,z), cell)
  | let dgLevels = groupBy (equating null) (lines input)
  , (z,level) <- zip [0..] (filter (not.null.head) dgLevels)
  , (y,row)   <- zip [0..] level
  , (x,cell)  <- zip [0..] row]

showDung :: [(Point,Char)] -> String
showDung = dung . sortBy (comparingZYX `on` fst) where
  comparingZYX = mconcat (map comparing [pz,py,px])
  dung = unlines . map level . groupBy (equating pz `on` fst)
  level = unlines . map row . groupBy (equating py `on` fst)
  row = map snd

neighbors :: Point -> Char -> [Point]
neighbors (x,y,z) 'U' = (x,y,z-1) : planar (x,y,z)
neighbors (x,y,z) 'D' = (x,y,z+1) : planar (x,y,z)
neighbors (x,y,z)  _  = planar (x,y,z)

planar :: Point -> [Point]
planar (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z)]

pathToNode :: (MonadPlus m, Eq a) => a -> Tree a -> m [a]
pathToNode x (Node y ys)
  | x == y    = return [y]
  | otherwise = fmap (y:) . msum . fmap (pathToNode x) $ ys

main :: IO ()
main = interact (maybe "No solution" showDung . challenge . readDung)

bfs :: Graph -> [Vertex] -> Forest Vertex
bfs graph start = evalState (unfoldForestM_BF go start) Set.empty where
  go :: Vertex -> State (Set.Set Vertex) (Vertex, [Vertex])
  go x = do
    visited <- get
    let follow = Set.fromList (graph ! x)
    let new = Set.difference follow visited
    put (Set.union visited new)
    return (x, Set.toList new)

challenge :: [(Point,Char)] -> Maybe [(Point,Char)]
challenge dung = do
  let dgEdges = [(cell, pos, neighbors pos cell) | (pos,cell) <- dung, cell /= '#']
  let (graph, fromVert, toVert) = graphFromEdges dgEdges
  (start, _) <- find ((=='S').snd) dung
  (goal,  _) <- find ((=='G').snd) dung
  startV <- toVert start
  goalV  <- toVert goal
  let vPaths = bfs graph [startV]
  soln <- msum $ map (pathToNode goalV) vPaths
  let crumbs = [(pos, '*') | (' ', pos, _) <- map fromVert soln]
  return $ unionBy (equating fst) crumbs dung
