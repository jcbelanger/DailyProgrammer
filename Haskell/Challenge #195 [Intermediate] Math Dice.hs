{-|
* [2014-12-31] Challenge #195 [Intermediate] Math Dice

http://www.reddit.com/r/dailyprogrammer/comments/2qxrtk/20141231_challenge_195_intermediate_math_dice/

Description:

Math Dice is a game where you use dice and number combinations to score. It's a neat way for kids to get mathematical dexterity. In the game, you first roll the 12-sided Target Die to get your target number, then roll the five 6-sided Scoring Dice. Using addition and/or subtraction, combine the Scoring Dice to match the target number. The number of dice you used to achieve the target number is your score for that round. For more information, see the product page for the game: (http://www.thinkfun.com/mathdice)
Input:

You'll be given the dimensions of the dice as NdX where N is the number of dice to roll and X is the size of the dice. In standard Math Dice Jr you have 1d12 and 5d6.
Output:

You should emit the dice you rolled and then the equation with the dice combined. E.g.
 9, 1 3 1 3 5

 3 + 3 + 5 - 1 - 1 = 9
Challenge Inputs:

 1d12 5d6
 1d20 10d6
 1d100 50d6
-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

fnMap = zip
    [(+), (-)]
    ["+", "-"]

calcResults (x:xs) = foldM (\a b -> fst <$> fnMap <*> [a] <*> [b]) x xs

showResults (x:xs) = foldM (\a b -> showStep <$> fnMap <*> [a] <*> [b]) (show x) xs
    where showStep f a b = unwords [a, snd f, show b]

combinations = nub . concatMap subsequences . permutations
        
solutions :: Int -> [Int] -> [String]
solutions target nums = [ show target ++ " = " ++ text
    | attempt <- combinations nums
    , not (null attempt)
    , (result, text) <- zip (calcResults attempt) (showResults attempt)
    , result == target ]

parseDie text = let (n, 'd' : x) = break (== 'd') text
                in  (read n, read x)

main = do
    [(n1, x1), (n2, x2)] <- map parseDie . words <$> getLine
    (target, g) <- randomR (1, x1) <$> getStdGen
    let nums = take n2 $ randomRs (1, x2) g
        answers = solutions target nums
    putStrLn $ show target ++ ", " ++ unwords (map show nums)
    putStrLn $ fromMaybe "No Solution" (listToMaybe answers)