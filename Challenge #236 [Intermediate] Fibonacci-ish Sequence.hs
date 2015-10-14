{-
https://www.reddit.com/r/dailyprogrammer/comments/3opin7/20151014_challenge_236_intermediate_fibonacciish/
http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibGen.html
-}

main = interact (unwords . map show . challenge . read)

challenge x = takeWhile (<=x) $ fib $ div x $ last $ filter ((==0).rem x) $ tail $ takeWhile (<=x) $ fib 1

fib n = 0 : n : zipWith (+) (fib n) (tail (fib n))
