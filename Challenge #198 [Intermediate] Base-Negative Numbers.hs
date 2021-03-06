{-|
* [2015-01-21] Challenge #198 [Intermediate] Base-Negative Numbers 

http://www.reddit.com/r/dailyprogrammer/comments/2t3m7j/20150121_challenge_198_intermediate_basenegative/

(Intermediate): Base-Negative Numbers
"Don't be stupid, Elite6809!", I hear you say. "You can't have a negative base." Well, why not? Let's analyse what we mean by base. Given a base-r system, the column p places from the right (starting from zero), which contains the digit n, has the value n×rp. The binary columns 1, 2, 4, 8, 16, ... is the same as 20, 21, 22, 23, 24. Nothing stops you from using a negative base with this system, except perhaps the understanding of the concept and practicality of its usage.
Let's imagine base -10 (negadecimal). Here, the place values for each column are now 1, -10, 100, -1000 and so on. Therefore, the negadecimal number 7211:
-Thousands    Hundreds    -Tens    Units
    7            2           1       1
 (-7000)   +   (200)   +   (-10) +  (1) = -6809
Is equivalent to -6809 in standard decimal.
Your challenge is, given a negative base and a value, convert it to the representation in the corresponding positive base, and vice versa.
Input and Output Description
Challenge Input
You will accept two numbers: r and n. n is a number in base r. For example:
-4 1302201
This input means 1302201 in base -4.
Challenge Output
Print the value of the input number in the corresponding opposite-signed base, for example, for the input above:
32201
As 1302201 in base -4 equals 32201 in base 4.
Sample Inputs and Outputs
Input: -10 12345678 (convert from base -10 to base 10)
Output: -8264462
Input:-7 4021553
Output: 4016423
Similarly, if the given base is positive, convert back to the corresponding negative base.
Input: 7 4016423 (convert from base 7 to base -7)
Output: 4021553
Input: 6 -3014515
Output: 13155121
Extension (Hard)
Extend your program to support imaginary bases. Imaginary bases can represent any complex number. The principle is the same; for example, base 4i can be used to represent complex numbers much the same way as a cartesian representation like a+bi. If you have forgotten the principles of imaginary numbers, re-read the challenge description for The Complex Number - you might want to re-use some code from that challenge anyway.
Notes
Try and do both the main challenge and extension without looking for the conversion algorithms on the internet. This is part of the challenge!
-}

import Data.Char
import Data.Complex --Extended Challenge: Imaginary numbers from Data.Complex work for toBase10.  Haven't figured out imaginary bases for fromBase10 yet.

toBase10 base ('-':num) = -(toBase10 base num)
toBase10 base num = sum $ zipWith (*) powers (reverse digits)
    where powers = map (base^) [0..]
          digits = map (fromIntegral . digitToInt) num

fromBase10 base num = sign ++ concatMap show digits
    where sign = if num < 0 then "-" else ""
          steps = takeWhile (not . isDone) $ iterate step (abs num, 0)
          step (q, r) = quotRem' q base
          isDone (q, r) = q==0 && r==0
          digits = reverse . map snd . tail $ steps

--Need quotRem to round toward zero with negative numbers.
quotRem' a b = let (x, y) = quotRem a b
               in if y < 0
                  then (x + 1, y + (abs b))
                  else (x, y)

main = interact $ \input -> let [b, num] = words input
                                base = read b
                                num10 = toBase10 base num
                            in fromBase10 (-base) num10

--unrelated factorial work--

{-

toFactoradic :: Int -> [Int]
toFactoradic = reverse . map snd . takeWhile notZeros . quotRems where
    quotRems n = zipWith quotRem (n:map fst (quotRems n)) [1..]
    notZeros (0,0) = False
    notZeros _     = True

fromFactoradic :: [Int] -> Int
fromFactoradic = sum . zipWith (*) (map factorial [0..]) . reverse where
    factorial n = product [1..n]
    
permutationsOfSize n = map (fromLehmerCode . zeroPad . toFactoradic) [0..] where
    zeroPad digits = replicate (n - length digits) 0 ++ digits

fromLehmerCode :: [Int] -> [Int]
fromLehmerCode code = toList . fst $ foldl' cutAt ([], avail) code where
    avail = Seq.fromList [0..length code-1]
    cutAt (xs, avail) ix = (x:xs, before >< after) where 
        (before, viewl -> x :< after) = Seq.splitAt ix avail

-}
