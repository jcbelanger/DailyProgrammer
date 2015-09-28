{-
https://www.reddit.com/r/dailyprogrammer/comments/3moxid/20150928_challenge_234_easy_vampire_numbers/
-}

import Data.List
import Control.Monad

main :: IO ()
main = interact $ \input ->
  let [m,n] = map read (words input)
      fangSize = m `div` n
  in unlines [ show vampire ++ "=" ++ intercalate "*" (map show fangs)
             | fangs <- replicateM n [10^(fangSize-1) .. 10^fangSize-1]
             , let vampire = product fangs
             , length (show vampire) == m
             , null (concatMap show fangs \\ show vampire)
             , any ((/=0) . (`rem` 10)) fangs ]
