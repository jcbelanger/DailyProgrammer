{-|
* [2014-12-31] Challenge #195 [Intermediate] Math Dice

http://www.reddit.com/r/dailyprogrammer/comments/2qxrtk/20141231_challenge_195_intermediate_math_dice/

This solution makes heavy use of applicative []'s for nondeterminism.
-}


import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import System.Random

fnMap = zip
    [(+), (-)]
    ["+", "-"]

subsets :: [a] -> [[a]]
subsets = filterM $ const [True, False]

calcResults :: [Int] -> [Int]
calcResults (x:xs) = foldM (\a b -> fst <$> fnMap <*> [a] <*> [b]) x xs

showResults :: [Int] -> [String]
showResults list = foldM (\a b -> showStep <$> fnMap <*> [a] <*> [b]) x xs
    where showStep f a b = unwords [a, snd f, b]
          x:xs = show <$> list
          
parseDie :: String -> (Int, Int)
parseDie = (read *** read . tail) . break (== 'd')

main = do
    [(n1, x1), (n2, x2)] <- fmap parseDie . words <$> getLine
    (target, g) <- randomR (1, x1) <$> getStdGen
    let nums = take n2 $ randomRs (1, x2) g
    putStrLn $ show target ++ ", " ++ unwords (show <$> nums)
    putStrLn $ maybe "No Solution" id $ listToMaybe [text ++ " = " ++ show target 
        | subset <- subsets nums
        , not (null subset)
        , (result, text) <- zip (calcResults subset) (showResults subset)
        , result == target ]