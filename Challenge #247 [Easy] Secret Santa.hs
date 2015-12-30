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
challenge [family] = shuffleM family
challenge families = do
  families' <- shuffleM families
  let half = sum (map length families) `div` 2
      Just (leftSize,(xss,yss)) = M.lookupLE half (splits families')
  xs <- shuffleM (concat xss)
  ys <- if half == leftSize
        then shuffleM (concat yss)
        else challenge yss
  let extra = drop leftSize ys
      path = concat [[x,y] | (x,y) <- zip xs ys]
  return (extra ++ path)

splits :: Eq a => [[a]] -> M.Map Int ([[a]],[[a]])
splits xss = foldl' transferLeft (M.singleton 0 ([],xss)) xss where
  transferLeft prev xs = M.union prev (nextSplit xs prev)
  nextSplit xs = M.mapKeysMonotonic (+length xs) . M.map (toLeft xs)
  toLeft xs (ass,bss) = (xs:ass, delete xs bss)
