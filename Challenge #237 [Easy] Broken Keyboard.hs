{-
https://www.reddit.com/r/dailyprogrammer/comments/3pcb3i/20151019_challenge_237_easy_broken_keyboard/
-}

import Data.List
import Data.Ord

main :: IO ()
main = do
  dict <- lines <$> readFile "enable1.txt"
  interact (unlines . map (challenge dict) . tail . lines)

challenge :: [String] -> [Char] -> String
challenge dict working = case filter (all (`elem` working)) dict of
  []     -> working ++ " can't type any words alone"
  remain -> working ++ " = " ++ maximumBy (comparing length) remain
