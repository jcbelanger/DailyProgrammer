{-
https://www.reddit.com/r/dailyprogrammer/comments/3ofsyb/20151012_challenge_236_easy_random_bag_system/cvxrt77
-}

{-# LANGUAGE ViewPatterns #-}

import Data.Sequence (ViewL(..), viewl, (><))
import qualified Data.Sequence as Seq
import Data.Foldable
import System.Random

possiblePieces :: String
possiblePieces = "OISZLJT"

main :: IO ()
main = putStrLn . take 50 . getPieces =<< getStdGen

getPieces :: StdGen -> String
getPieces gen =
  let numPossible = factorial (length possiblePieces)
      factorial n = product [1..n]
  in  concatMap (permutationOf possiblePieces) (randomRs (0, numPossible-1) gen)

permutationOf :: [a] -> Int -> [a]
permutationOf xs n =
  let code = zeroPadTo (length xs) (toFactoradic n)
  in  fromLehmerCode code xs

toFactoradic :: Int -> [Int]
toFactoradic = reverse . map snd . takeWhile notZeros . quotRems where
  quotRems n = zipWith quotRem (n:map fst (quotRems n)) [1..]
  notZeros (0,0) = False
  notZeros _     = True

zeroPadTo :: Int -> [Int] -> [Int]
zeroPadTo size digits = replicate (size - length digits) 0 ++ digits

fromLehmerCode :: [Int] -> [a] -> [a]
fromLehmerCode code xs = fst $ foldl' cutAt ([], Seq.fromList xs) code where
    cutAt (ys, avail) ix =
      let (before, viewl -> y :< after) = Seq.splitAt ix avail
      in  (y:ys, before >< after)
