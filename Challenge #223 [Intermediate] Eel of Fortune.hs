import Control.Monad
import Data.List

problem secret offensive = offensive == filter (`elem` offensive) secret

challenge1 offensive = length . filter (`problem` offensive) . lines <$> readFile "enable1.txt"

challenge2 = do
    enable1 <- readFile "enable1.txt"
    let problemCount word = length . filter (`problem` word) $ lines enable1
    return . take 10 . qsort . map problemCount $ replicateM 5 ['a'..'z']

qsort []     = []
qsort (x:xs) = zip2nd gt (qsort gt) ++ [x] ++ zip2nd lt (qsort lt) where
    lt = filter ( < x) xs
    gt = filter (>= x) xs
    zip2nd []      _      = []
    zip2nd (_:xs) ~(y:ys) = y:zip2nd xs ys
