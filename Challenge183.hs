{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
* [10/06/2014] Challenge #183 [Easy] Semantic Version Sort
http://www.reddit.com/r/dailyprogrammer/comments/2igfj9/10062014_challenge_183_easy_semantic_version_sort/

This solution makes use of applicative parsing and monoids for sorting
-}

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
          <*> optional (char '-' *> text)
          <*> optional (char '+' *> text)
          where text = many1 $ letter <|> digit

main :: IO ()
main = interact challenge183

challenge183 :: Text -> Text
challenge183 = either (const "Failed to parse input") formatOutput . parseOnly parseInput

parseInput :: Parser [SemVer]
parseInput = do n <- decimal <* endOfLine
                count n $ parseSemVer <* (endOfLine <|> endOfInput)

formatOutput :: [SemVer] -> Text
formatOutput = intercalate "\n" . map (pack . show) . sort