{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Challenge183 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.List            hiding (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text            hiding (count, head, map)
import Data.Text.IO         (interact)
import Prelude              hiding (interact)

data SemVer = SemVer
            { major :: Int
            , minor :: Int
            , patch :: Int
            , label :: Maybe String
            , meta  :: Maybe String
            } deriving (Eq)

instance Ord SemVer where
    compare = comparing major
           <> comparing minor
           <> comparing patch
           <> comparing (isNothing . label)

instance (Show SemVer) where
    show (SemVer {..}) = show major
                      ++ "."
                      ++ show minor
                      ++ "."
                      ++ show patch
                      ++ maybe "" ("-" ++) label
                      ++ maybe "" ("+" ++) meta

parseSemVer :: Parser SemVer
parseSemVer = SemVer
          <$> decimal
          <*> (char '.' *> decimal)
          <*> (char '.' *> decimal)
          <*> optional (char '-' *> many1 alphaNum)
          <*> optional (char '+' *> many1 alphaNum)
          where alphaNum = letter <|> digit

main :: IO ()
main = interact challenge183

challenge183 :: Text -> Text
challenge183 = either (const "Failed to parse input") formatOutput . parseOnly parseInput

parseInput :: Parser [SemVer]
parseInput = do n <- decimal <* endOfLine
                count n $ parseSemVer <* (endOfLine <|> endOfInput)

formatOutput :: [SemVer] -> Text
formatOutput = intercalate "\n" . map (pack . show) . sort
