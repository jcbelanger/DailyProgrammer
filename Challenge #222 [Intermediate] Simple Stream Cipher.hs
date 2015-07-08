import System.Random
import Data.Bits

encrypt :: Int -> String -> [Int]
encrypt key = zipWith xor (generator key) . map fromEnum

decrypt :: Int -> [Int] -> String
decrypt key = map toEnum . zipWith xor (generator key)

generator :: Int -> [Int]
generator = iterate (lcg 128 664525 1013904223)

lcg :: Int -> Int -> Int -> Int -> Int
lcg m a c x = (a * x + c) `mod` m

generator2 :: Int -> [Int]
generator2 = randoms . mkStdGen
