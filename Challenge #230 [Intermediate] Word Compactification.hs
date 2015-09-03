{-
https://www.reddit.com/r/dailyprogrammer/comments/3jd72z/20150901_challenge_230_intermediate_word/
-}

import Data.List
import Data.Function
import qualified Data.Map as Map
import Control.Monad
import Data.Ord

type Grid = Map.Map (Int,Int) Char

main = interact $ printGrid . minimumBy (comparing Map.size) . (attempts <=< permutations . byCommas)

byCommas :: String -> [String]
byCommas input = case break (==',') input of
    (before, ',':after) -> before : byCommas after
    (before, _)         -> [before]

bounds :: Grid -> (Int,Int,Int,Int)
bounds grid | (xs,ys) <- unzip (Map.keys grid) = (minimum xs,minimum ys,maximum xs,maximum ys)

printGrid :: Grid -> String
printGrid grid = unlines [ [ maybe ' ' id (Map.lookup (x,y) grid) | x <- [minX..maxX]]
                         | let (minX,minY,maxX,maxY) = bounds grid
                         , y <- [minY..maxY]]

horizontal x y word = Map.fromList [((x+dx,y),char) | (dx,char) <- zip [0..] word]
vertical   x y word = Map.fromList [((x,y+dy),char) | (dy,char) <- zip [0..] word]

attempts :: [String] -> [Grid]
attempts (x:xs) = foldM go (horizontal 0 0 x) xs where
    go :: Grid -> String -> [Grid]
    go grid word = [ Map.union grid attempt
                   | let w = length word
                   , let (minX,minY,maxX,maxY) = bounds grid
                   , (place,xs,ys) <- [ (horizontal,[minX-w..maxY+1],[minY-1..maxY+1])
                                      , (vertical,  [minX-1..maxY+1],[minY-w..maxY+1]) ]
                   , attempt <- place <$> xs <*> ys <*> [word, reverse word]
                   , and (Map.intersectionWith (==) attempt grid) ]
