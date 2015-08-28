{-
https://www.reddit.com/r/dailyprogrammer/comments/3i99w8/20150824_challenge_229_easy_the_dottie_number/
-}
converge = until =<< ((==) =<<)
challenge = converge cos
optional1 = converge (\x -> x - tan x) 2
optional2 = converge (\x -> 1 + 1/x)
optional3 = converge (\x -> 4*x*(1-x))
