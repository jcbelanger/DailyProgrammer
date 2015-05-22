{-
http://www.reddit.com/r/dailyprogrammer/comments/36m83a/20150520_challenge_215_intermediate_validating/
-}

import qualified Data.Vector as V

main = interact $ \input ->
    let [wordCount, _]:compares = [map read (words line) | line <- lines input]
        allSorted = all isSorted [ foldl runCompare testNum compares
                                 | testNum <- V.replicateM wordCount [0,1] ]
    in if allSorted 
       then "Valid network"
       else "Invalid network"

isSorted xs = V.and $ V.zipWith (<=) xs (V.drop 1 xs)

runCompare testNum [startIx, endIx] = do
    a <- V.indexM testNum startIx
    b <- V.indexM testNum endIx
    testNum V.// [ (startIx, min a b), (endIx, max a b) ]
