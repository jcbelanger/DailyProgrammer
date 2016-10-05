{-
https://www.reddit.com/r/dailyprogrammer/comments/55zdxx/20161005_challenge_286_intermediate_zeckendorf/
-}

phi :: Double
phi = (sqrt 5 + 1) / 2

binetFib :: Int -> Int
binetFib n = round $ (phi^n - (-1)^n / phi^n) / sqrt 5

fibIx :: Int -> Double
fibIx n = (log (fromIntegral n) + log 5 / 2) / log phi

largestFib :: Int -> Int
largestFib n =
    let ix = fibIx n
        [lo, hi] = binetFib <$> [floor ix, ceiling ix]
    in if hi > n then lo else hi

zeckendorf :: Int -> [Int]
zeckendorf n =
    let steps = iterate step (n, largestFib n)
        step (r,f) = let r' = r-f in (r', largestFib r')
    in snd <$> takeWhile ((>0).fst) steps

main :: IO ()
main = interact (unwords . map show . zeckendorf . read)
