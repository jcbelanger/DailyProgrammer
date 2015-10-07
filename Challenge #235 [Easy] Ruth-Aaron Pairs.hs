{-
https://www.reddit.com/r/dailyprogrammer/comments/3nkanm/20151005_challenge_235_easy_ruthaaron_pairs/
-}

main :: IO ()
main = interact (unlines . map (test . read) . tail . lines)

isPrime :: Int->Bool
isPrime x = all ((/=0).(mod x)) [2..floor (sqrt (fromIntegral x))]

primeFactors :: Int -> [Int]
primeFactors n = [x | x <- [2..n], n `mod` x == 0, isPrime x]

ruthAaron :: (Int, Int) -> String
ruthAaron (x, y) | sum (primeFactors x) == sum (primeFactors y) = show (x,y) ++ " VALID"
                 | otherwise                                    = show (x,y) ++ " NOT VALID"
