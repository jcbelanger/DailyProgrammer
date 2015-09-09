{-
https://www.reddit.com/r/dailyprogrammer/comments/3jz8tt/20150907_challenge_213_easy_cellular_automata/
-}

import Data.Char
import Data.List

xor :: Int -> Int -> Int
xor x y = (x + y) `rem` 2

rule90 :: [Int] -> [Int]
rule90 xs = [xor a c | a:b:c:_ <- tails ([0]++xs++[0])]

toString :: [Int] -> String
toString = map toChar
  where toChar 1 = 'x'
        toChar _ = ' '

main :: IO ()
main = interact $ unlines . map toString . take 25 . iterate rule90 . map digitToInt
