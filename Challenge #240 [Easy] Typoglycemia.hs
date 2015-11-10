{-
https://www.reddit.com/r/dailyprogrammer/comments/3s4nyq/20151109_challenge_240_easy_typoglycemia/
-}

import           Data.Char
import           Data.IntMap   (IntMap, (!))
import qualified Data.IntMap   as IM
import           Data.List
import           System.Random

main :: IO ()
main = do
  gen <- getStdGen
  interact (unwords . map (fst . typoglycemia gen) . words)

typoglycemia :: RandomGen g => g -> String -> (String, g)
typoglycemia gen xs = (IM.elems result, gen') where
  ixs = zip [0..] xs
  (midAlpha, preserve) = case partition (isAlpha.snd) ixs of
    (y:ys@(_:_), notAlpha) -> (init ys, y:last ys:notAlpha)
    (alpha, notAlpha)      -> ([], ixs)
  (midIx', gen') = fisherYates gen (map fst midAlpha)
  mid' = zip midIx' (map snd midAlpha)
  result = IM.unions (map IM.fromList [mid',preserve])

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen []     = ([], gen)
fisherYates gen (x:xs) = (IM.elems result, gen') where
  (result, gen') = foldl swap (IM.singleton 0 x, gen) (zip [1..] xs)

swap :: RandomGen g => (IntMap a, g) -> (Int, a) -> (IntMap a, g)
swap (xs, gen) (i, x) = (IM.insert j x (IM.insert i (xs ! j) xs), gen')
  where (j, gen') = randomR (0, i) gen
