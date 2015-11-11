{-
https://www.reddit.com/r/dailyprogrammer/comments/3s4nyq/20151109_challenge_240_easy_typoglycemia/
-}

import           Control.Monad
import           Control.Monad.Random
import           Data.Char
import           Data.IntMap          (IntMap, (!))
import qualified Data.IntMap          as IM
import           Data.List

main :: IO ()
main = putStrLn =<< evalRandIO . challenge =<< getContents

challenge :: MonadRandom m => String -> m String
challenge = fmap unwords . mapM typoglycemia . words

typoglycemia :: MonadRandom m => String -> m String
typoglycemia xs = do
  case partition (isAlpha.snd) (zip [0..] xs) of
    (y:ys@(_:_), notAlpha) -> do
      let (mid, preserve) = (init ys, y:last ys:notAlpha)
      midIx' <- fisherYates (map fst mid)
      let mid' = zip midIx' (map snd mid)
      return $ map snd $ sortOn fst (preserve ++ mid')
    _                      -> return xs

fisherYates :: MonadRandom m => [a] -> m [a]
fisherYates []     = return []
fisherYates (x:xs) = IM.elems <$> foldM swap (IM.singleton 0 x) (zip [1..] xs)

swap ::  MonadRandom m => IntMap a -> (Int, a) -> m (IntMap a)
swap xs (i, x) = do
  j <- getRandomR (0, i)
  return $ IM.insert j x (IM.insert i (xs ! j) xs)
