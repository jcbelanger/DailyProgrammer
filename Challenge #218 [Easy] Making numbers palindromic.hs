{-
http://www.reddit.com/r/dailyprogrammer/comments/38yy9s/20150608_challenge_218_easy_making_numbers/
-}

import qualified Data.Map as Map
import Text.Printf

isPal xs = xs == reverse xs

step xs = show $ read xs + read (reverse xs)

challenge n = let (steps, pal:_) = break isPal (iterate step n)
              in printf "%s gets palindromatic after %d steps: %s" n (length steps) pal

main = interact challenge

bonus1 = foldr logPal Map.empty nums where
    nums = filter (not . isLychrel) (map show [1..1000])
    logPal n = Map.insertWith (++) (pal n) [n]
    pal = until isPal step

isLychrel = null . snd . break isPal . take 1000 . iterate step

bonus2 = filter isLychrel (map show [1..])
