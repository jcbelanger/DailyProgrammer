{-
[2015-03-11] Challenge #205 [Intermediate] RPN

http://www.reddit.com/r/dailyprogrammer/comments/2yquvm/20150311_challenge_205_intermediate_rpn/

Description:

My father owned a very old HP calculator. It was in reverse polish notation (RPN). He would hand me his calculator and tell me "Go ahead and use it". Of course I did not know RPN so everytime I tried I failed.
So for this challenge we will help out young coder_d00d. We will take a normal math equation and convert it into RPN. Next week we will work on the time machine to be able to send back the RPN of the math equation to past me so I can use the calculator correctly.
Input:

A string that represents a math equation to be solved. We will allow the 4 functions, use of () for ordering and thats it. Note white space between characters could be inconsistent.
Number is a number
"+" add
"-" subtract
"/" divide
"x" or "*" for multiply
"(" with a matching ")" for ordering our operations
Output:

The RPN (reverse polish notation) of the math equation.
Challenge inputs:

Note: "" marks the limit of string and not meant to be parsed.
 "0+1"
 "20-18"
 " 3               x                  1   "
 " 100    /                25"
 " 5000         /  ((1+1) / 2) * 1000"
 " 10 * 6 x 10 / 100"
 " (1 + 7 x 7) / 5 - 3  "
 "10000 / ( 9 x 9 + 20 -1)-92"
 "4+5 * (333x3 /      9-110                                      )"
 " 0 x (2000 / 4 * 5 / 1 * (1 x 10))"
Additional Challenge:

Since you already got RPN - solve the equations.
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative

data Exp
    = Number Double
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp

rpn :: Exp -> String
rpn (Number n) = show n
rpn (Add a b)  = unwords [rpn a, rpn b, "+"]
rpn (Sub a b)  = unwords [rpn a, rpn b, "-"]
rpn (Mul a b)  = unwords [rpn a, rpn b, "*"]
rpn (Div a b)  = unwords [rpn a, rpn b, "/"]

eval :: Exp -> Double
eval (Number n) = n
eval (Add a b)  = eval a + eval b
eval (Sub a b)  = eval a - eval b
eval (Mul a b)  = eval a * eval b
eval (Div a b)  = eval a / eval b

{- LL grammar used to support left associativity:

<exp> -> <factor> <expTail>

<expTail> -> - <term> <expTail>
           | + <term> <expTail>
           | <eof>

<factor> -> <term> <factorTail>

<factorTail> -> * <term> <factorTail>
              | / <term> <factorTail>
              | <eof>

<term> -> ( <exp> )
        | <number>
        
-}
        
expP :: Parser Exp
expP = headForm factorP expTailP

expTailP :: Parser (Maybe (Exp -> Exp))
expTailP =  charPad '+' *> tailForm Add factorP expTailP
        <|> charPad '-' *> tailForm Sub factorP expTailP
        <|> pure Nothing

factorP :: Parser Exp
factorP = headForm termP factorTailP

factorTailP :: Parser (Maybe (Exp -> Exp))
factorTailP =   charPad '/' *> tailForm Div termP factorTailP
           <|> (charPad '*' <|> charPad 'x') *> tailForm Mul termP factorTailP
           <|> pure Nothing

termP :: Parser Exp
termP =  charPad '(' *> expP <* charPad ')'
     <|> Number <$> pad double

charPad :: Char -> Parser Char
charPad = pad . char

pad :: Parser a -> Parser a
pad p = skipSpace *> p <* skipSpace

headForm headP tailP = do
    left <- headP
    mTail <- tailP
    return $ case mTail of
        Just tail -> tail left
        Nothing -> left

tailForm f rightP tailP = do
    right <- rightP
    mTail <- tailP
    let head x = f x right
    return . Just $ case mTail of
        Just tail -> tail . head
        Nothing   -> head

main = TIO.interact $ \input ->
    case parseOnly (expP <* endOfInput) input of
        Right exp -> T.pack $ rpn exp ++ " = " ++ show (eval exp)
        _         -> "Failed to parse"
