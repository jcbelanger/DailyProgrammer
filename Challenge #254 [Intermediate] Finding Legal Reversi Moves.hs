import Text.Printf
import Data.Map as Map
import Data.Set as Set

type Pos = (Int,Int)
type Piece = Char

main :: IO ()
main = interact $ \input ->
  let [turn]:remain = lines input
      pieces = Map.fromList [ ((x,y),val)
                            | (y,row) <- zip [1..] remain
                            , (x,val) <- zip [1..] row
                            , val /= '-' ]
      avail = Map.fromList  [ (edge, '*')
                            | edge <- Set.toList (edges pieces)
                            , dir <- directions
                            , let path = tail [Map.lookup pos pieces | pos <- iterate dir edge]
                            , case break (==Just turn) (takeWhile (/=Nothing) path) of
                               (_:_, _:_) -> True
                               _          -> False ]
      result = Map.union pieces avail
      info = printf "%d legal moves for %c" (Map.size avail) turn
      display = [[Map.findWithDefault '-' (x,y) result | x <- [1..8]] | y <- [1..8]]
  in unlines (info:display)

edges :: Map Pos Piece -> Set Pos
edges pieces = Set.fromList [ pos'
                            | pos <- Map.keys pieces
                            , dir <- directions
                            , let pos' = dir pos
                            , Map.notMember pos' pieces ]

directions :: [Pos -> Pos]
directions = [up,down,left,right,up.right,up.left,down.right,down.left]
  where
    up    (x,y) = (x,y-1)
    down  (x,y) = (x,y+1)
    left  (x,y) = (x-1,y)
    right (x,y) = (x+1,y)
