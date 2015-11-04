{-
https://www.reddit.com/r/dailyprogrammer/comments/3r7wxz/20151102_challenge_239_easy_a_game_of_threes/
-}

threes :: Int -> [[Int]]
threes 1 = [[1]]
threes n = [n, 1 - r] : threes q where (q, r) = (n + 1) `quotRem` 3

main :: IO ()
main = interact (unlines . map (unwords . map show) . threes . read)
