{-
https://www.reddit.com/r/dailyprogrammer/comments/3oz82g/20151016_challenge_236_hard_balancing_chemical/
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.List
import qualified Data.Map             as Map
import qualified Data.Text            as T

main :: IO ()
main = print . challenge =<< readFile "in.txt"

challenge :: String -> String
challenge input = case parseOnly (equationP <* endOfInput) (T.pack input) of
  Left msg -> "Failed to parse: " ++ msg
  Right equation -> case balance equation of
    Left msg -> "Could not balance: " ++ msg
    Right balacned -> show balacned

data Equation = Equation [Substance] [Substance]
data Substance = Substance Int [Group]
data Group = Atom String Int | Compound [Group] Int

instance Show Equation where
  show (Equation reactants products) = intercalate " + " (map show reactants) ++ " -> " ++ intercalate " + " (map show products)

instance Show Substance where
  show (Substance 1 groups) = concatMap show groups
  show (Substance coefficient groups) = show coefficient ++ concatMap show groups

instance Show Group where
  show (Atom element 1) = element
  show (Atom element subscript) = element ++ show subscript
  show (Compound groups 1) = concatMap show groups
  show (Compound groups subscript) = "(" ++ concatMap show groups ++ ")" ++ show subscript

equationP :: Parser Equation
equationP = Equation <$> substancesP <* string "->" <*> substancesP where
  substancesP = (skipSpace *> substanceP <* skipSpace) `sepBy` char '+'

substanceP :: Parser Substance
substanceP  = Substance <$> option 1 decimal <* skipSpace <*> some groupP

groupP :: Parser Group
groupP = Atom <$> elementP <*> option 1 decimal
     <|> Compound <$> groupsP <*> option 1 decimal
     where groupsP = char '(' *> some groupP <* char ')'
                 <|> char '[' *> some groupP <* char ']'

elementP :: Parser String
elementP = (:) <$> satisfy isUpper <*> many (satisfy isLower)

balance :: Equation -> Either String Equation
balance (Equation reactants products) = Left "not impl"
