{-
https://www.reddit.com/r/dailyprogrammer/comments/6vi9ro/170823_challenge_328_intermediate_pyramid_sliding/
-}

main :: IO ()
main = interact (show . challenge . tail . map (map read . words) . lines)

challenge :: [[Int]] -> Int
challenge = head . foldr1 (\row prev -> zipWith (+) row (zipWith min prev (tail prev)))
