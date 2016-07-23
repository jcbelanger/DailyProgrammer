{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Function
import           Data.Ratio
import           Data.Text               (Text)
import           Text.Parser.Combinators (chainl1, chainr1)
import           Text.Printf

type Equation = (Expr, Expr)

data Expr
    = Lit Integer
    | Var Char
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Exp Expr Expr
    | Neg Expr
    deriving (Eq, Ord)

instance Show Expr where
    show (Lit x)   = show x
    show (Var x)   = [x]
    show (Add x y) = printf "(%s + %s)" (show x) (show y)
    show (Sub x y) = printf "(%s - %s)" (show x) (show y)
    show (Mul x y) = printf "(%s * %s)" (show x) (show y)
    show (Div x y) = printf "(%s / %s)" (show x) (show y)
    show (Exp x y) = printf "(%s ^ %s)" (show x) (show y)
    show (Neg x)   = printf "-(%s)"     (show x)

showRational :: Rational -> String
showRational x 
    | denominator x == 1 = show (numerator x)
    | otherwise          = printf "(%s/%s)" (show (numerator x)) (show (denominator x))


main :: IO ()
main = print $ toEquation "3*x^(y-2)^2/4=2"

fromExp = putStrLn . either ("error:"++) show . parseOnly (expr <* endOfInput)

toEquation :: Text -> Either String Equation
toEquation = parseOnly (equation <* endOfInput)

equation :: Parser Equation
equation = (,) <$> expr <* string "=" <*> expr

expr,term,fact,prim,lit,var,neg,parens :: Parser Expr
expr = term `chainl1` (addFn <|> subFn)
term = fact `chainl1` (mulFn <|> divFn)
fact = prim `chainr1` expFn
prim = choice [lit, var, parens, neg]
lit = Lit <$> decimal
var = Var <$> satisfy isAlpha
parens = char '(' *> expr <* char ')'
neg = Neg <$ char '-' <*> expr

addFn,subFn,mulFn,divFn,expFn :: Parser (Expr -> Expr -> Expr)
addFn = Add <$ char '+'
subFn = Sub <$ char '-'
mulFn = Mul <$ char '*'
divFn = Div <$ char '/'
expFn = Exp <$ char '^'

valid :: Equation -> Bool
valid = uncurry ((==) `on` toSOP)

--TODO IMPLEMENT!!!!
toSOP :: Expr -> Expr
toSOP = id
