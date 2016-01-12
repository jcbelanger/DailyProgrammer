{-
https://www.reddit.com/r/dailyprogrammer/comments/40h9pd/20160111_challenge_249_easy_playing_the_stock/
-}

main :: IO ()
main = interact (show . challenge . map read . words)

challenge :: [Double] -> (Double, Double)
challenge prices = maximumBy (comparing $ uncurry subtract)
      [ (buy, sell)
      | (buy, _:sells) <- zip prices (tails prices)
      , sell <- sells]
