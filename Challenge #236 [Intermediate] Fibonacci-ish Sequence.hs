main = interact (unwords . map show . challenge . read)

challenge x = takeWhile (<=x) $ fib $ div x $ last $ filter ((==0).rem x) $ tail $ takeWhile (<=x) $ fib 1

fib n = 0 : n : zipWith (+) (fib n) (tail (fib n))
