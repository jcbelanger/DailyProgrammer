{-
https://www.reddit.com/r/dailyprogrammer/comments/4jom3a/20160516_challenge_267_easy_all_the_places_your/
-}

import Data.List
import Data.Ord

main :: IO ()
main = interact $ intercalate ", " . map toOrdinal . (`delete` [0..100]) . read

toOrdinal :: Int -> String
toOrdinal n = show n ++ suffix special
  where
    suffix = maybe "th" snd . find (endsWith . fst) . sortOn (Down . fst)
    special = [(11,"th"),(12,"th"),(13,"th"),(1,"st"),(2,"nd"),(3,"rd")]
    endsWith x = x == n `rem` 10 ^ digitCount x
    digitCount = (+1) . floor . logBase 10 . fromIntegral
