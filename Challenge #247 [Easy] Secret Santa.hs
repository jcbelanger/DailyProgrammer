{-
https://www.reddit.com/r/dailyprogrammer/comments/3yiy2d/20151228_challenge_247_easy_secret_santa/
-}

import           Data.List
import qualified Data.Set      as S
import           System.Random

main :: IO ()
main = do
  gen <- getStdGen
  interact (maybe "No solution"  fst . selectFrom gen . solutions)

selectFrom :: RandomGen g => g -> [a] -> Maybe (a, g)
selectFrom _   [] = Nothing
selectFrom gen xs =
  let (ix, gen') = randomR (0, length xs) gen
  in Just (xs !! ix, gen')

nextWith :: (a -> a -> b) -> [a] -> [b]
nextWith f xs = zipWith f xs (drop 1 (cycle xs))

showGives :: String -> String -> String
showGives a b = a ++ " -> " ++ b

solutions :: String -> [String]
solutions input =
  [ unlines $ nextWith showGives names
  | attempt <- permutations [ ((fid,nid),name)
                            | (fid,family) <- zip [1..] (lines input)
                            , (nid,name)   <- zip [1..] (words family) ]
  , let (ids, names) = unzip attempt
  , and $ nextWith (/=) $ map fst ids
  , and $ nextWith (/=) $ scanl' (flip S.insert) S.empty ids ]
