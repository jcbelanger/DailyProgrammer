{-
https://www.reddit.com/r/dailyprogrammer/comments/3uuhdk/20151130_challenge_243_easy_abundant_and/
-}

main :: IO ()
main = interact (unlines . map (challenge . read) . lines)

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `rem` d == 0]

challenge :: Int -> String
challenge n = case compare delta 0 of
  LT -> "deficient by " ++ show (abs delta)
  GT -> "abundant by " ++ show delta
  EQ -> "perfect"
  where delta = 2*n - sum (divisors n)
