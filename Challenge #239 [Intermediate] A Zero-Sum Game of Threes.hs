{-
https://www.reddit.com/r/dailyprogrammer/comments/3rhzdj/20151104_challenge_239_intermediate_a_zerosum/
-}
{-# LANGUAGE DeriveFunctor, BangPatterns #-}

import Data.List

type Threes = [[Integer]]

main :: IO ()
main = interact (maybe "Impossible" showThrees . challenge . read)

challenge :: Integer -> Maybe Threes
challenge = find ((==0) . sum . map (!!1) . init) . fastThrees

showThrees :: Threes -> String
showThrees = unlines . map (unwords . map show)

threes :: (Integer -> [Threes]) -> Integer -> [Threes]
threes _ 1 = [[[1]]]
threes f n =
  [ [n,dn]:after
  | dn <- sortOn abs [-2..2]
  , let (q, r) = (n + dn) `quotRem` 3
  , r == 0 && q > 0
  , after <- f q ]

threesTree :: Tree [Threes]
threesTree = threes fastThrees <$> nats

fastThrees :: Integer -> [Threes]
fastThrees = index threesTree

data Tree a = Tree (Tree a) a (Tree a) deriving (Functor)

index :: Tree a -> Integer -> a
index (Tree _    val _    ) 0 = val
index (Tree left _   right) n = case (n - 1) `quotRem` 2 of
    (q,0) -> index left  q
    (q,1) -> index right q

nats :: Tree Integer
nats = go 0 1 where
  go !nat !total = Tree (go left total') nat (go right total')
    where (left, right, total') = (nat+total, left+total, total*2)
