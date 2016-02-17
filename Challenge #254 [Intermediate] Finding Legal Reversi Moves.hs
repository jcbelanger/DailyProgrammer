import Text.Printf
import Data.Maybe
import Data.Ix

main :: IO ()
main = interact $ \input ->
  let [turn]:remain = lines input
      pieces = [ ((x,y),val)
               | (y,row) <- zip [1..] remain
               , (x,val) <- zip [1..] row
               , val /= '-' ]
      avail =  [ (far,'*')
               | (pos,val) <- pieces
               , val == turn
               , dir <- directions
               , let (near,far) = (dir pos, dir near)
               , inRange ((1,1),(8,8)) far
               , maybe False (/=turn) (lookup near pieces)
               , isNothing (lookup far pieces) ]
      info = printf "%d legal moves for %c" (length avail) turn
      result = [[ fromMaybe '-' $ lookup (x,y) (pieces ++ avail)
                | x <- [1..8] ]
                | y <- [1..8] ]
  in unlines (info:result)

directions :: [(Int,Int) -> (Int,Int)]
directions = [up,down,left,right,up.right,up.left,down.right,down.left]
  where
    up    (x,y) = (x,y-1)
    down  (x,y) = (x,y+1)
    left  (x,y) = (x-1,y)
    right (x,y) = (x+1,y)
