{-
https://www.reddit.com/r/dailyprogrammer/comments/557wyy/20160930_challenge_285_hard_math_proofs/
-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TIO
import           Text.Parser.Combinators   (chainl1)
import           Text.Printf

data Equation = Equation Expr Expr deriving (Eq, Ord)

instance Show Equation where
    show (Equation a b) = printf "%s=%s" (show a) (show b)

data Expr
    = Lit Double
    | Var Char
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Poly Polynomial --only used for printint
    | PolyR PolyRational --only used for printing
    deriving (Eq, Ord)

instance Show Expr where
    show (Lit x)   = showDouble x
    show (Var x)   = [x]
    show (Neg x)   = printf "-(%s)" (show x)
    show (Add a b) = printf "(%s+%s)" (show a) (show b)
    show (Sub a b) = printf "(%s-%s)" (show a) (show b)
    show (Mul a b) = printf "%s*%s" (show a) (show b)
    show (Div a b) = printf "%s/%s" (show a) (show b)
    show (Poly a)  = printf "(%s)" (show a)
    show (PolyR a) = printf "(%s)" (show a)

showDouble :: Double -> String
showDouble x | fromIntegral (floor x) == x = show (floor x)
             | otherwise                   = show x

instance Num Expr where
    (+) = Add
    (*) = Mul
    (-) = Sub
    negate = Neg
    fromInteger = Lit . fromInteger

instance Fractional Expr where
    (/) = Div

parseEquation :: Text -> Either String Equation
parseEquation = parseOnly (equation <* endOfLine) . Text.filter (/=' ')

equation :: Parser Equation
equation = Equation <$> expr <* char '=' <*> expr

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
newtype Polynomial = Polynomial { terms :: Map (Map Variable Exponent) Coefficient } deriving (Eq, Ord)

instance Show Polynomial where
    show (Polynomial ts) = intercalate "+"
        [ if coef == 1 && t /= []
          then concat t
          else showDouble coef ++ concat t
        | (varPows, coef) <- Map.toList ts
        , let t = [ if pow == 1
                    then [var]
                    else var:"^"++showDouble pow
                  | (var,pow) <- Map.toList varPows ]]


litP :: Coefficient -> Polynomial
litP = Polynomial . Map.singleton Map.empty

varP :: Variable -> Polynomial
varP a = Polynomial $ Map.singleton (Map.singleton a 1) 1

instance Num Polynomial where
    negate = Polynomial . Map.map negate . terms
    fromInteger = Polynomial . Map.singleton Map.empty . fromInteger
    (Polynomial a) + (Polynomial b) = Polynomial $ Map.unionWith (+) a b
    (Polynomial a) * (Polynomial b) = Polynomial $ Map.fromList
        [ (Map.unionWith (+) varPowA varPowB, coefA * coefB)
        | (varPowA, coefA) <- Map.toList a
        , (varPowB, coefB) <- Map.toList b ]

data PolyRational = PolyRational Polynomial Polynomial deriving (Eq, Ord)

instance Show PolyRational where
    show (PolyRational a 1) = show a
    show (PolyRational a b) = show a ++ "/" ++ show b

instance Num PolyRational where
    fromInteger a = PolyRational (fromInteger a) 1
    negate (PolyRational a b) = PolyRational (negate a) b
    (PolyRational a b) + (PolyRational c d) = PolyRational (a*b*d+c*b*d) (b*d)
    (PolyRational a b) * (PolyRational c d) = PolyRational (a*c) (b*d)

instance Fractional PolyRational where
    (PolyRational a b) / (PolyRational c d) = PolyRational (a*d) (b*c)

type Proof s = Writer [(s,String)]

stepCensor :: (s -> s') -> Proof s a -> Proof s' a
stepCensor f = mapWriter $ \(x,steps) -> (x, [(f stmt, reason) | (stmt, reason) <- steps])

tellStep :: Eq s => s -> String -> Proof s ()
tellStep stmt reason = tell [(stmt, reason)]

fromExpr :: Expr -> Proof Expr PolyRational
fromExpr (Lit a) = return $ PolyRational (litP a) 1
fromExpr (Var a) = return $ PolyRational (varP a) 1
fromExpr (Neg a) = do
    a' <- stepCensor Neg (fromExpr a)
    let ret = negate a'
    tellStep (PolyR ret) "Negate"
    return ret
fromExpr (Add a b) = do
    let withB x = Add x b
    a' <- stepCensor withB  (fromExpr a)
    let withA' = Add (PolyR a')
    b' <- stepCensor withA' (fromExpr b)
    let ret = a' + b'
    tellStep (PolyR ret) "Add Terms"
    return ret
fromExpr (Mul a b) = do
    let withB x = Mul x b
    a' <- stepCensor withB  (fromExpr a)
    let withA' = Mul (PolyR a')
    b' <- stepCensor withA' (fromExpr b)
    let ret = a' * b'
    tellStep (PolyR ret) "Multiply Terms"
    return ret
fromExpr (Sub a b) = do
    let withB x = Sub x b
    a' <- stepCensor withB  (fromExpr a)
    let withA' = Sub (PolyR a')
    b' <- stepCensor withA' (fromExpr b)
    let ret = a' - b'
    tellStep (PolyR ret) "Subtract Terms"
    return ret
fromExpr (Div a b) = do
    let withB x = Div x b
    a' <- stepCensor withB  (fromExpr a)
    let withA' = Div (PolyR a')
    b' <- stepCensor withA' (fromExpr b)
    let ret = a' / b'
    tellStep (PolyR ret) "Divide Fraction"
    return ret

equivalent :: Equation -> Proof Equation Bool
equivalent eqn@(Equation l r) = do
    tellStep eqn "Given"
    let withR x = Equation x r
    l'@(PolyRational a b) <- stepCensor withR  (fromExpr l)
    let withL' = Equation (PolyR l')
    r'@(PolyRational c d) <- stepCensor withL' (fromExpr r)
    let (ad,bc) = (a*d,b*c)
    when (b /= 1 && d /= 1) $ tellStep (Equation (Poly ad) (Poly bc)) "Cross Product"
    let withBC x = Equation x (Poly bc)
    ad' <- stepCensor withBC (trivialTerms ad)
    let withAD' = Equation (Poly ad')
    bc' <- stepCensor withAD' (trivialTerms bc)
    return (ad' == bc')

zeroCoef, zeroExp :: Polynomial -> Polynomial
zeroCoef = Polynomial . Map.filter (/=0) . terms
zeroExp  = Polynomial . Map.mapKeys (Map.filter (/=0)) . terms

trivialTerms :: Polynomial -> Proof Expr Polynomial
trivialTerms a = do
    let b = zeroExp a
    when (a /= b) $ tellStep (Poly b) "Zero Exponenet"
    let c = zeroCoef b
    when (b /= c) $ tellStep (Poly c) "Zero Term"
    return c

showProof :: Proof Equation Bool -> String
showProof proof =
    let (equiv, steps) = runWriter proof
        result = if equiv then "Equivilent:" else "Not Equivilent:"
    in unlines $ result:[printf "%s\t\t%s" (show stmt) reason | (stmt, reason) <- steps]

main :: IO ()
main = interact $ either error (showProof . equivalent) . parseEquation . Text.pack
