{-
https://www.reddit.com/r/dailyprogrammer/comments/3yiy2d/20151228_challenge_247_easy_secret_santa/
-}

import           Control.Monad.Random
import           Data.List
import qualified Data.Map              as M
import           System.Random.Shuffle

main :: IO ()
main = do
  gen <- getStdGen
  interact ((`evalRand` gen) . fmap showResults . challenge . toFamilies)

toFamilies :: String -> [[String]]
toFamilies = map words . lines

showResults :: [String] -> String
showResults xs = unlines [a++" -> "++b | (a,b) <- zip xs (drop 1 $ cycle xs)]

challenge :: MonadRandom m => [[String]] -> m [String]
challenge families = do
  families' <- shuffleM families
  let total = sum (map length families)
      Just (_,(xs,ys)) = M.lookupLE (total `div` 2) (splits families')
      --TODO allow inter-family-gifting when no perfect split
  xs' <- shuffleM xs
  ys' <- shuffleM ys
  return $ concat [[x,y] | (x,y) <- zip (concat xs') (concat ys')]

splits :: Eq a => [[a]] -> M.Map Int ([[a]],[[a]])
splits xss = foldl' transferLeft (M.singleton 0 ([],xss)) xss where
  transferLeft prev cur = M.union prev (nextSplit cur prev)
  nextSplit cur = M.mapKeysMonotonic (+length cur) . M.map (toLeft cur)
  toLeft x (as,bs) = (x:as, delete x bs)
