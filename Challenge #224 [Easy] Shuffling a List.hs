{-
https://www.reddit.com/r/dailyprogrammer/comments/3e0hmh/20150720_challenge_224_easy_shuffling_a_list/
-}

import Data.IntMap (IntMap, elems, insert, singleton, (!))
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  interact (unwords . fst . fisherYates gen . words)

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen []     = ([], gen)
fisherYates gen (x:xs) = (elems result, gen') where
  (result, gen') = foldl swap (singleton 0 x, gen) (zip [1..] xs)
  swap :: RandomGen g => (IntMap a, g) -> (Int, a) -> (IntMap a, g)
  swap (xs, gen) (i, x) = (insert j x (insert i (xs ! j) xs), gen')
    where (j, gen') = randomR (0, i) gen
