{-
https://www.reddit.com/r/dailyprogrammer/comments/3l61vx/20150916_challenge_232_intermediate_where_should/
-}

import Data.List
import Data.Ord

main :: IO ()
main = interact $ show . minimumBy (comparing $ uncurry dist) . pairs . parse

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

pairs :: [a] -> [(a,a)]
pairs set = [(x,y) | (x:xs) <- tails set, y <- xs]

parse :: String -> [(Double,Double)]
parse = map read . tail . lines
