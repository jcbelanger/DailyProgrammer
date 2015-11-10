{-
https://www.reddit.com/r/dailyprogrammer/comments/3s4nyq/20151109_challenge_240_easy_typoglycemia/
-}

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Data.Char
import System.Random
import Data.List

main :: IO ()
main = do
  gen <- getStdGen
  interact (unwords . map (fst . typoglycemia gen) . words)

typoglycemia :: RandomGen g => g -> String -> (String, g)
typoglycemia gen xs@(_:_:_) = (IM.elems result, gen') where
  ixs = zip [0..] xs
  (begin, mid, end) = (head ixs, init (tail ixs), last ixs)
  (alpha, notAlpha) = partition (isAlpha.snd) mid
  (alphaIx', gen') = fisherYates gen (map fst alpha)
  alpha' = zip alphaIx' (map snd alpha)
  result = IM.unions (map IM.fromList [alpha',notAlpha,[begin,end]])
typoglycemia gen xs       = (xs, gen)

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen []     = ([], gen)
fisherYates gen (x:xs) = (IM.elems result, gen') where
  (result, gen') = foldl swap (IM.singleton 0 x, gen) (zip [1..] xs)

swap :: RandomGen g => (IntMap a, g) -> (Int, a) -> (IntMap a, g)
swap (xs, gen) (i, x) = (IM.insert j x (IM.insert i (xs ! j) xs), gen')
  where (j, gen') = randomR (0, i) gen
