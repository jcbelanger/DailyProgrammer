import Data.Function
import Data.List
import Data.Ord

similar = on (==)

optimals = head . groupBy (similar head) . sortBy (comparing head) <$> groupBy (similar length) golombs

golombs = [0]:[ marks'
              | marks@(x:xs) <- golombs
              , let n = length marks + 1
              , mark <- [x+1..2^n-1]
              , let marks' = mark:marks
              , let dists = [ y - x | (x:xs) <- tails marks', y <- xs ]
              , length dists == length (nub dists) ]
