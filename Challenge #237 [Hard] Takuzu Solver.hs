import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List
import           Data.Map          (Map)
import qualified Data.Map.Strict   as Map
import           Data.Maybe
import           Data.Tuple

type Cell = Char
type MapGrid = Map (Int,Int) Cell
type ListGrid = [[Maybe Cell]]
type App a = ReaderT Int (State MapGrid) a

main :: IO ()
main = interact challenge

challenge :: String -> String
challenge input =
  let size = length (lines input)
      grid = Map.fromList [ ((x,y), char)
                          | (y, line) <- zip [1..] (lines input)
                          , (x, char) <- zip [1..] line
                          , char /= '.' ]
      grid' = execState (runReaderT solve size) grid
  in unlines [[fromMaybe '.' (Map.lookup (x,y) grid')
              | x <- [1..size]]
              | y <- [1..size]]

solve :: App ()
solve = do
  analytical
  cols <- colListGrid
  let remain = [(x,y) | (x,col) <- zip [1..] cols, (y,Nothing) <- zip [1..] col]
  when (remain /= []) $ do
    let empty = head remain
    before <- get
    modify (Map.insert empty '0')
    solve
    valid <- validGrid
    unless valid $ do
      put before
      modify (Map.insert empty '1')
      solve

analytical :: App ()
analytical = do
  before <- get
  checkRunsOf3
  checkBandCounts
  --checkUniqueRows --I'm lazy and will just rely on backtracking
  after <- get
  unless (before == after) analytical

validGrid :: App Bool
validGrid = do
  size <- ask
  grid <- get
  cols <- colListGrid
  rows <- rowListGrid
  return $ and [ full && chunksOf2 && counts && unique
               | bands <- [cols,rows]
               , band <- bands
               , let full = size * size == Map.size grid
               , let chunksOf2 = all ((<=2).length) (group band)
               , let half = size `div` 2
               , let counts = all ((==half).length) (group $ sort band)
               , let unique = bands == nub bands]

colListGrid :: App ListGrid
colListGrid = do
  size <- ask
  grid <- get
  return [[Map.lookup (x,y) grid | y <- [1..size]] | x <- [1..size]]

rowListGrid :: App ListGrid
rowListGrid = transpose <$> colListGrid

runOfThree :: [Maybe Cell] -> Maybe (Cell,Int)
runOfThree (Nothing:Just a:Just b:_) | a == b = Just (a,0)
runOfThree (Just a:Nothing:Just b:_) | a == b = Just (a,1)
runOfThree (Just a:Just b:Nothing:_) | a == b = Just (a,2)
runOfThree _                                  = Nothing

flipCell :: Cell -> Cell
flipCell '1' = '0'
flipCell '0' = '1'

checkRunsOf3 :: App ()
checkRunsOf3 = do
  cols <- colListGrid
  rows <- rowListGrid
  sequence_ [ modify (Map.insert pos' val')
            | (bands,posFix) <- [(rows,id),(cols,swap)]
            , (y,band) <- zip [1..] bands
            , (x,Just (val,dx)) <- zip [1..] (runOfThree <$> tails band)
            , let pos' = posFix (x+dx,y)
            , let val' = flipCell val ]

checkBandCounts :: App ()
checkBandCounts = do
  size <- ask
  cols <- colListGrid
  rows <- rowListGrid
  sequence_ [ modify (Map.insert pos' val')
            | (bands,posFix) <- [(rows,id),(cols,swap)]
            , (y,band) <- zip [1..] bands
            , (x,Nothing) <- zip [1..] band
            , val <- "01"
            , length (filter (==Just val) band) == size `div` 2
            , let pos' = posFix (x,y)
            , let val' = flipCell val]
