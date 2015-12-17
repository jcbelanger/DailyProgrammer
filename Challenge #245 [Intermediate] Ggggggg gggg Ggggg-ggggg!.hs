import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Text            as T

gggMapping :: Parser (Parser Char)
gggMapping = do
  to <- anyChar
  space
  from <- takeTill isSpace
  return (string from >> return to)

decode :: Parser String
decode = do
  gggMappings <- gggMapping `sepBy1` space
  endOfLine
  manyTill (choice gggMappings <|> anyChar) endOfLine

main :: IO ()
main = interact (either ("error durring parsing: " ++) id . parseOnly decode . T.pack)
