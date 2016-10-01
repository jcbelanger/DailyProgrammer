{-
https://www.reddit.com/r/dailyprogrammer/comments/557wyy/20160930_challenge_285_hard_math_proofs/
-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bool
import           Data.Char
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TIO
import           Text.Parser.Combinators   (chainl1)

type Equation = (Expr, Expr)
data Expr
    = Lit Double
    | Var Char
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    deriving (Eq, Ord, Show)

parseEquation :: Text -> Either String Equation
parseEquation = parseOnly (equation <* endOfLine) . Text.filter (/=' ')

equation :: Parser Equation
equation = (,) <$> expr <* string "=" <*> expr

expr,term,fact,prim,lit,var,neg,parens :: Parser Expr
expr = term <|> neg
term = fact `chainl1` (addFn <|> subFn)
fact = prim `chainl1` (divFn <|> mulFn)
prim = lit <|> var <|> parens
lit = Lit <$> unsigned double
var = Var <$> satisfy isAlpha
neg = Neg <$ char '-' <*> expr
parens = char '(' *> expr <* char ')'

unsigned :: Parser a -> Parser a
unsigned f = do
    sign <- optional (char '+' <|> char '-')
    if sign == Nothing then f else empty

addFn,subFn,mulFn,divFn :: Parser (Expr -> Expr -> Expr)
addFn = Add <$ char '+'
subFn = Sub <$ char '-'
mulFn = Mul <$ optional (char '*')
divFn = Div <$ char '/'

type Variable = Char
type Exponent = Double
type Coefficient = Double
type Terms = Map (Map Variable Exponent) Coefficient
newtype Polynomial = Polynomial { terms :: Terms } deriving (Eq, Ord)

--Smart constructor to remove trivial terms
poly :: Terms -> Polynomial
poly = Polynomial . Map.mapKeys (Map.filter (/=0)) . Map.filter (/=0)

fromExpr :: Expr -> Polynomial
fromExpr (Lit a)   = poly $ Map.singleton Map.empty  a
fromExpr (Var a)   = poly $ Map.singleton (Map.singleton a 1) 1
fromExpr (Neg a)   = negateP (fromExpr a)
fromExpr (Add a b) = fromExpr a `addP` fromExpr b
fromExpr (Mul a b) = fromExpr a `mulP` fromExpr b
fromExpr (Sub a b) = fromExpr a `addP` negateP (fromExpr b)
fromExpr (Div a b) = fromExpr a `mulP` fromDenominator (fromExpr b)

negateP :: Polynomial -> Polynomial
negateP = poly . Map.map negate . terms

fromDenominator :: Polynomial -> Polynomial
fromDenominator = poly . Map.mapKeys (Map.map negate) . Map.map recip . terms

addP :: Polynomial -> Polynomial -> Polynomial
addP (Polynomial a) (Polynomial b) = poly $ Map.unionWith (+) a b

mulP :: Polynomial -> Polynomial -> Polynomial
mulP (Polynomial a) (Polynomial b) = poly $ Map.fromList
    [ (Map.unionWith (+) varPowA varPowB, coefA * coefB)
    | (varPowA, coefA) <- Map.toList a
    , (varPowB, coefB) <- Map.toList b ]

equivalent :: Equation -> Bool
equivalent (a,b) = fromExpr a == fromExpr b

main :: IO ()
main = TIO.interact $ either error (bool "Not Equivilent" "Equivilent" . equivalent) . parseEquation
