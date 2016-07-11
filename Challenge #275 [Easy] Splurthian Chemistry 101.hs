{-
https://www.reddit.com/r/dailyprogrammer/comments/4savyr/20160711_challenge_275_easy_splurthian_chemistry/
-}

import Data.Char
import Data.List

newtype Element = Element String deriving (Eq, Ord, Show)
data Symbol = Symbol Char Char deriving (Eq, Ord, Show)

challenge :: Element -> Symbol -> Bool
challenge = flip elem . symbols

symbols :: Element -> [Symbol]
symbols (Element name) = [Symbol (toUpper x) (toLower y) | x:xs <- tails name, y:_ <- tails xs]

bonus :: Element -> Symbol
bonus = minimum . symbols
