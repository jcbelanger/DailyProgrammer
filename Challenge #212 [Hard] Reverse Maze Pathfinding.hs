{-
http://www.reddit.com/r/dailyprogrammer/comments/34izkl/20150501_challenge_212_hard_reverse_maze/
-}

import Data.Char
import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.State
import Text.Printf

data Action = TurnLeft | TurnRight | Forward Int
data Direction = North | East | South | West deriving (Enum, Bounded)
type MazeState = (Pos, Direction)
type Pos = (Int, Int)

main = interact $ \input ->
    let n:remain = lines input
        (maze, [directions]) = splitAt (read n) remain
        spaces = Set.fromList [ (x, y) 
                              | (y, line) <- zip [0..] maze
                              , (x, ' ')  <- zip [0..] line ]
        steps = parseActions directions
    in unlines [ printf "From %s to %s" (show startPos) (show endPos)
               | startPos <- Set.toList spaces
               , startDir <- [minBound..maxBound]
               , let (path, (endPos, endDir)) = runSteps steps startPos startDir
               , path `Set.isSubsetOf` spaces ]

parseActions = map parse . groupBy ((&&) `on` isDigit) where
    parse "l" = TurnLeft
    parse "r" = TurnRight
    parse  n  = Forward (read n)

runSteps :: [Action] -> Pos -> Direction -> (Set Pos, MazeState)
runSteps steps startPos startDir = runState stepSpaces (startPos, startDir) where
    stepSpaces = foldM act (Set.singleton startPos) steps

act :: Set Pos -> Action -> State MazeState (Set Pos)
act xs (Forward n) = (Set.union xs) . Set.fromList <$> replicateM n (step >> gets fst)
act xs TurnLeft    = turn 1    >> return xs
act xs TurnRight   = turn (-1) >> return xs

turn :: Int -> State MazeState ()
turn delta = modify $ \(pos, dir) -> 
    let curIx = fromEnum dir
        newIx = (curIx + delta) `mod` 4
        newDir = toEnum newIx
    in  (pos, newDir)

step :: State MazeState ()
step = modify $ \((x,y), dir) ->
    let newPos = case dir of
            North -> (x, y+1)
            South -> (x, y-1)
            East  -> (x+1, y)
            West  -> (x-1, y)
    in (newPos, dir)

