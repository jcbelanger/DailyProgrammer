import Data.List
import Data.Function
import Data.Ord

optimals = 
    let golombs = [0]:[ marks'
                      | marks@(x:xs) <- golombs
                      , let n = length marks + 1
                      , mark <- [x+1..2^n-1]
                      , let marks' = mark:marks
                      , let dists = [ y - x 
                                    | (x:xs) <- tails marks'
                                    , y <- xs ]
                      , length dists == length (nub dists) ]
        byOrder = groupBy ((==) `on` length) golombs
        bySize = sortBy (comparing head) <$> byOrder
    in head . groupBy ((==) `on` head) <$> bySize
