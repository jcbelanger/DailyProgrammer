{-
https://www.reddit.com/r/dailyprogrammer/comments/4htg9t/20160504_challenge_265_easy_permutations_and/
-}

import Control.Monad
import Data.List

toPermutation :: Int -> Int -> Maybe [Int]
toPermutation x = fromLehmerCode [0..x - 1] . zeroPadTo x . toFactoradic

fromPermutation :: Int -> [Int] -> Maybe Int
fromPermutation x = fmap fromFactoradic . toLehmerCode [0..x - 1]

toFactoradic :: Int -> [Int]
toFactoradic = reverse . map snd . takeWhile notZeros . quotRems
  where
    quotRems :: Int -> [(Int,Int)]
    quotRems n =
        let quots = n:[q | (q,r) <- quotRems n]
        in zipWith quotRem quots [1..]

    --Allows takeWhile to include the remainder when the quotient reaches zero
    notZeros :: (Int,Int) -> Bool
    notZeros (0,0) = False
    notZeros _     = True

fromFactoradic :: [Int] -> Int
fromFactoradic =
    let factorials = scanl (*) 1 [1..]
    in sum . zipWith (*) factorials . reverse

zeroPadTo :: Int -> [Int] -> [Int]
zeroPadTo size digits =
    let padSize = size - length digits
    in replicate padSize 0 ++ digits

fromLehmerCode :: [a] -> [Int] -> Maybe [a]
fromLehmerCode xs = fmap (reverse . fst) . foldM cutAt ([], xs)
  where
    cutAt :: ([a], [a]) -> Int -> Maybe ([a], [a])
    cutAt (ys, avail) iy
        | (before, y:after) <- splitAt iy avail = Just (y:ys, before ++ after)
        | otherwise                             = Nothing

toLehmerCode :: Eq a => [a] -> [a] -> Maybe [Int]
toLehmerCode xs = fmap (reverse . fst) . foldM lookupCut ([], xs)
  where
    lookupCut :: Eq a => ([Int], [a]) -> a -> Maybe ([Int], [a])
    lookupCut (iys, remain) y = do
        iy <- elemIndex y remain
        return (iy:iys, delete y remain)
