{-
[2015-02-16] Challenge #202 [Easy] I AM BENDER. PLEASE INSERT GIRDER.

http://www.reddit.com/r/dailyprogrammer/comments/2w84hl/20150216_challenge_202_easy_i_am_bender_please/

Description

Poor Mr.Tinkles is having some troubles. Similar to The Loneliest Whale In The World, no one can hear his cries. Or in this case, understand them.

He talks in a sequence of on's and off's. 0's and 1's, it's binary. Obviously as a mere human you can't possibly translate what he's saying as he says it. Looks like you'll have to think of a way around this....
Formal Inputs & Outputs
Input description

On console input you will be given a variable number of 0's and 1's that correspond to letters in the alphabet [a-z] and whitespace ' '. These will be integers coming in, it's your job to cast them however you need.
Output description

The program should output the english translation (or other languages if you feel so inclinced!) of the binary phrase
Samples

Input

010010000110010101101100011011000110111100100
0000101011101101111011100100110110001100100

Output

Hello World

Test Input
1

011100000110110001100101011000

010111001101100101001000000111

010001100001011011000110101100

100000011101000110111100100000

0110110101100101
2

011011000110100101100110011001

010010000001110010011010010110

011101101000011101000010000001

101110011011110111011100100000

011010010111001100100000011011

000110111101101110011001010110

110001111001

-}

import Data.Char (chr, digitToInt)
import Data.List.Split (chunksOf)

toBase10 = sum . zipWith (*) powers . digits
    where digits = reverse . map digitToInt
          powers = map (2^) [0..]

main = interact $ map (chr . toBase10) . chunksOf 8
