{-
https://www.reddit.com/r/dailyprogrammer/comments/557wyy/20160930_challenge_285_hard_math_proofs/
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bool
import           Data.Char
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as TIO
import           Text.Parser.Combinators (chainl1)

type Equation = (Expr, Expr)
data Expr
    = Lit Integer
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
lit = Lit <$> decimal
var = Var <$> satisfy isAlpha
parens = char '(' *> expr <* char ')'
neg = Neg <$ char '-' <*> expr

addFn,subFn,mulFn,divFn :: Parser (Expr -> Expr -> Expr)
addFn = Add <$ char '+'
subFn = Sub <$ char '-'
mulFn = Mul <$ optional (char '*')
divFn = Div <$ char '/'

newtype Polynomial = Polynomial { terms :: Map (Map Char Rational) Rational } deriving (Eq, Ord, Show)

--Smart constructor to remove trivial terms
poly :: Map (Map Char Rational) Rational -> Polynomial
poly = Polynomial . Map.mapKeys (Map.filter (/=0)) . Map.filter (/=0)

fromExpr :: Expr -> Polynomial
fromExpr (Neg a)   = fromExpr $ Mul (Lit (-1)) a
fromExpr (Sub a b) = fromExpr $ Add a (Neg b)
fromExpr (Lit a)   = poly $ Map.singleton Map.empty (toRational a)
fromExpr (Var a)   = poly $ Map.singleton (Map.singleton a 1) 1
fromExpr (Add a b) = fromExpr a `add` fromExpr b
fromExpr (Mul a b) = fromExpr a `mul` fromExpr b
fromExpr (Div a b) = fromExpr a `mul` recripPoly (fromExpr b)

recripPoly :: Polynomial -> Polynomial
recripPoly = poly . Map.mapKeys (Map.map negate) . Map.map (1/) . terms

add, mul :: Polynomial -> Polynomial -> Polynomial
add (Polynomial a) (Polynomial b) = poly $ Map.unionWith (+) a b
mul (Polynomial a) (Polynomial b) = poly $ Map.fromList
    [ (Map.unionWith (+) varsA varsB, coefA * coefB)
    | (varsA, coefA) <- Map.toList a
    , (varsB, coefB) <- Map.toList b ]

equivalent :: Equation -> Bool
equivalent (a,b) = fromExpr a == fromExpr b

main :: IO ()
main = TIO.interact $ either error (bool "Not Equivilent" "Equivilent" . equivalent) . parseEquation
