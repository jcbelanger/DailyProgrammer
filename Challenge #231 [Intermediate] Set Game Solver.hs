{-
https://www.reddit.com/r/dailyprogrammer/comments/3ke4l6/20150909_challenge_231_intermediate_set_game/
-}

import Data.List

sets xs = [ x | x@[_,_,_] <- subsequences xs, all ((/= 2) . length . nub) (transpose x)]

main = interact (unlines . map unwords . sets . lines)
