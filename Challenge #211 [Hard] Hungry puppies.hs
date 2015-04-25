{-
http://www.reddit.com/r/dailyprogrammer/comments/33ow0c/20150424_challenge_211_hard_hungry_puppies/
-}

import Data.List
import Data.Ord
import Control.Arrow
import Control.Parallel.Strategies

main = interact $ unwords . map show . parMax . permutations . map read . words

parMax = fst . maximumBy (comparing snd) . withStrategy (parBuffer 64 rdeepseq) . map (id &&& score)

score :: [Int] -> Int
score xs@(a:b:_) = start + body + end
    where body = sum . map bodyScore $ zip3 xs (tail xs) (tail $ tail xs)
          start = edgeScore a b
          end = let v:u:_ = reverse xs in edgeScore v u
          edgeScore x y | x > y     = 1
                        | x < y     = -1
                        | otherwise = 0
          bodyScore (x,y,z) | x < y && y > z = 1
                            | x > y && y < z = -1
                            | otherwise      = 0
score _ = 0
