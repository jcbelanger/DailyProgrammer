{-
https://www.reddit.com/r/dailyprogrammer/comments/3v4zsf/20151202_challenge_243_intermediate_jennys_fruit/
-}

import Control.Monad (foldM)
import Data.List (intercalate)

main :: IO ()
main = interact (unlines . map showBasket . challenge . parse)

parse :: String -> [(String,Int)]
parse input = [(fruit, read count) | [fruit,count] <- words <$> lines input]

showBasket :: [(String,Int)] -> String
showBasket counts = intercalate ", " [ show count ++ " " ++ plural count fruit
                                     | (fruit,count) <- counts
                                     , count /= 0 ]

plural :: Int -> String -> String
plural 1 = id
plural _ = (++"s")

challenge :: [(String,Int)] -> [[(String,Int)]]
challenge prices = filter ((==500) . basketCost prices) (foldM go [] prices) where
  go :: [(String,Int)] -> (String,Int) -> [[(String,Int)]]
  go counts (fruit,price) = [ counts'
                            | count <- [0..500 `div` price]
                            , let counts' = (fruit,count):counts
                            , basketCost prices counts' <= 500 ]

basketCost :: [(String,Int)] -> [(String,Int)] -> Int
basketCost prices counts = sum [maybe 0 (count*) (lookup fruit prices) | (fruit,count) <- counts]
