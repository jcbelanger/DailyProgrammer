{-
https://www.reddit.com/r/dailyprogrammer/comments/3d4fwj/20150713_challenge_223_easy_garland_words/
-}

import Data.List

garland word = length . head $ filter (`isPrefixOf` word) (tail $ tails word)

challenge1 word = word ++ cycle (drop (garland word) word)

challenge2 = maximum . map garland . lines <$> readFile "enable1.txt"
