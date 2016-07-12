{-
https://www.reddit.com/r/dailyprogrammer/comments/4savyr/20160711_challenge_275_easy_splurthian_chemistry/
-}

module Challenge where

import Data.Char
import Data.List

type Element = String
type Symbol = String

challenge :: Element -> Symbol -> Bool
challenge = flip elem . splurth

splurth :: Element -> [Symbol]
splurth element = [[toUpper x, toLower y] | x:xs <- tails element, y <- xs]

bonus1 :: Element -> Symbol
bonus1 = minimum . splurth

bonus2 :: Element -> Int
bonus2 = length . nub . splurth

blurth :: Element -> [Symbol]
blurth element = [toUpper x : map toLower xs | x:xs <- subsequences element]

bonus3 :: Element -> Int
bonus3 = length . nub . blurth
