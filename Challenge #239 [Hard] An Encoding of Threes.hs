{-
https://www.reddit.com/r/dailyprogrammer/comments/3rrtxh/20151106_challenge_239_hard_an_encoding_of_threes/
-}

{-# LANGUAGE BangPatterns, DeriveFunctor #-}

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Monoid

type Threes = [[Integer]]

data Trie a = Trie
  { leaf   :: Bool
  , follow :: Map a (Trie a) }

instance Ord a => Monoid (Trie a) where
  mempty = Trie False Map.empty
  mappend (Trie l1 f1) (Trie l2 f2) = Trie (l1 || l2) (Map.unionWith (<>) f1 f2)

fromList :: Ord a => [a] -> Trie a
fromList = foldr concatT emptyT where
  concatT x xs = Trie False (Map.singleton x xs)
  emptyT = Trie True Map.empty

member :: (Traversable t, Ord a) => t a -> Trie a -> Bool
member xs trie = maybe False leaf (foldM follows trie xs) where
  follows cur x = Map.lookup x (follow cur)

main :: IO ()
main = do
  file <- readFile "enable1.txt"
  let dict = foldMap fromList [map toLower word | word <- lines file]
  interact (unlines . challenge dict . read)

challenge :: Trie Char -> Integer -> [String]
challenge dict n =
  [ word
  | threeWord <- map (abs . (!!1)) . init  <$> fastThrees n
  , let word = chr . fromBase3 <$> chunksOf 5 threeWord
  , map toLower word `member` dict ]

fromBase3 :: [Integer] -> Int
fromBase3 = fromInteger . foldl' step 0 where
  step !total !digit = 3 * total + digit

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
    (q,_) -> index right q

nats :: Tree Integer
nats = go 0 1 where
  go !nat !total = Tree (go left total') nat (go right total')
    where (left, right, total') = (nat+total, left+total, total*2)
