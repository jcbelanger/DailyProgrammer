{-
https://www.reddit.com/r/dailyprogrammer/comments/3yiy2d/20151228_challenge_247_easy_secret_santa/
-}

import Data.List
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  interact (fst . selectFrom gen . solutions)

selectFrom :: RandomGen g => g -> [a] -> (a,g)
selectFrom gen xs =
  let (ix, gen') = randomR (0, length xs) gen
  in (xs !! ix, gen')

nextWith :: (a -> a -> b) -> [a] -> [b]
nextWith f xs = zipWith f xs (drop 1 (cycle xs))

showGives :: String -> String -> String
showGives a b = a ++ " -> " ++ b

solutions :: String -> [String]
solutions input =
  [ unlines $ nextWith showGives $ map snd attempt
  | attempt <- permutations [ (fid,name)
                            | (fid,family) <- zip [1..] (lines input)
                            , name <- words family ]
  , and $ nextWith (/=) $ map fst attempt]
