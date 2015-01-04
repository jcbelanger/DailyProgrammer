{-# LANGUAGE BangPatterns #-}

{-|
* [2014-12-31] Challenge #195 [Intermediate] Math Dice

http://www.reddit.com/r/dailyprogrammer/comments/2qxrtk/20141231_challenge_195_intermediate_math_dice/

This solution makes use of applicative []'s for non-determinism.
-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import System.Random

fnMap = zip
    [(+), (-), (*), div, (^)]
    ["+", "-", "*", "/", "^"]

calcResults :: [Int] -> [Int]
calcResults (x:xs) = foldM (\ !a !b -> fst <$> fnMap <*> [a] <*> [b]) x xs

showResults :: [Int] -> [String]
showResults list = foldM (\a b -> showStep <$> fnMap <*> [a] <*> [b]) x xs
    where showStep f a b = unwords [a, snd f, b]
          x:xs = show <$> list

solutions :: Int -> [Int] -> [String]
solutions target nums = [ text ++ " = " ++ show target
    | attempt <- nub [ perm
        | seq <- subsequences nums
        , perm <- permutations seq ]
    , not (null attempt)
    , (result, text) <- zip (calcResults attempt) (showResults attempt)
    , result == target ]
    `using` parBuffer 64 rdeepseq
    
parseDie :: String -> (Int, Int)
parseDie = (read *** read . tail) . break (== 'd')

main = do
    [(n1, x1), (n2, x2)] <- fmap parseDie . words <$> getLine
    (target, g) <- randomR (1, x1) <$> getStdGen
    let nums = take n2 $ randomRs (1, x2) g
        answers = solutions target nums
    putStrLn $ show target ++ ", " ++ unwords (show <$> nums)
    putStrLn $ fromMaybe "No Solution" (listToMaybe answers)