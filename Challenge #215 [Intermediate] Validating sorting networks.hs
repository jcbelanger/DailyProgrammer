{-
http://www.reddit.com/r/dailyprogrammer/comments/36m83a/20150520_challenge_215_intermediate_validating/
-}

import Data.List (foldl')
import Control.Monad.Par
import Control.Monad (foldM)
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

type Comparator = (Int, Int)

main = interact $ \input ->
    let metaLine:compareLines = lines input
        wordCount:_ = map read (words metaLine)
        sortingNetwork = runComparators (map parseComparator compareLines)
    in if all isSorted [sortingNetwork test | test <- V.replicateM wordCount [0,1]]
       then "Valid network"
       else "Invalid network"

parseComparator :: String -> Comparator
parseComparator line = let [start, end] = map read (words line) in (start, end)

isSorted :: Vector Int -> Bool
isSorted xs = V.and $ V.zipWith (<=) xs (V.drop 1 xs)

runComparators :: [Comparator] -> Vector Int -> Vector Int
runComparators comparators vect = runPar $ do
    iVect <- V.mapM newFull vect
    result <- foldM runComparator iVect comparators
    V.mapM get result

runComparator :: Vector (IVar Int) -> Comparator -> Par (Vector (IVar Int))
runComparator before (startIx, endIx) = do
    [small, large] <- sequence [new, new]
    fork $ do        
        a <- get (before ! startIx)
        b <- get (before ! endIx)
        put small (min a b)
        put large (max a b)
    return $ before // [(startIx, small), (endIx, large)]
