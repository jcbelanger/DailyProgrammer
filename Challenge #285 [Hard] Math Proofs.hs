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
import Data.Ratio

type Equation = (Expr, Expr)
data Expr
    = Lit Coefficient
    | Var Variable
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
newtype Polynomial = Polynomial { terms :: Terms } deriving (Eq, Ord, Show)

--Smart constructor to remove trivial terms
poly :: Terms -> Polynomial
poly = Polynomial . Map.mapKeys (Map.filter (/=0)) . Map.filter (/=0)

instance Num Polynomial where
    negate = poly . Map.map negate . terms
    fromInteger = poly . Map.singleton Map.empty . fromInteger
    (Polynomial a) + (Polynomial b) = poly $ Map.unionWith (+) a b
    (Polynomial a) * (Polynomial b) = poly $ Map.fromList
        [ (Map.unionWith (+) varPowA varPowB, coefA * coefB)
        | (varPowA, coefA) <- Map.toList a
        , (varPowB, coefB) <- Map.toList b ]

data PolyRational = PolyRational
    { prNumerator   :: Polynomial
    , prDenominator :: Polynomial
    } deriving (Ord, Show)

instance Eq PolyRational where
    (PolyRational a b) == (PolyRational c d) = a*d == b*c

instance Num PolyRational where
    fromInteger a = PolyRational (fromInteger a) 1
    negate (PolyRational a b) = PolyRational (negate a) b
    (PolyRational a b) + (PolyRational c d) = PolyRational (a*b*d+c*b*d) (b*d)
    (PolyRational a b) * (PolyRational c d) = PolyRational (a*c) (b*d)

instance Fractional PolyRational where
    (PolyRational a b) / (PolyRational c d) = PolyRational (a*d) (b*c)

litP :: Coefficient -> Polynomial
litP = poly . Map.singleton Map.empty

varP :: Variable -> Polynomial
varP a = poly $ Map.singleton (Map.singleton a 1) 1

fromExpr :: Expr -> PolyRational
fromExpr (Lit a)   = PolyRational (litP a) 1
fromExpr (Var a)   = PolyRational (varP a) 1
fromExpr (Neg a)   = negate (fromExpr a)
fromExpr (Add a b) = fromExpr a + fromExpr b
fromExpr (Mul a b) = fromExpr a * fromExpr b
fromExpr (Sub a b) = fromExpr a - fromExpr b
fromExpr (Div a b) = fromExpr a / fromExpr b

fromDenominator :: Polynomial -> Polynomial
fromDenominator = poly . Map.mapKeys (Map.map negate) . Map.map recip . terms

equivalent :: Equation -> Bool
equivalent (a,b) = fromExpr a == fromExpr b

main :: IO ()
main = TIO.interact $ either error (bool "Not Equivilent" "Equivilent" . equivalent) . parseEquation
