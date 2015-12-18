import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.List
import           Data.Ord
import qualified Data.Text            as T

challenge1 :: String -> String
challenge1 = either ("Error during parsing: " ++) id . parseOnly decode . T.pack

challenge2 :: String -> String
challenge2 input =
  let encoding = huffman "gG" . frequencies . filter isAlpha $ input
      encode ch | Just ch' <- lookup ch encoding = ch'
                | otherwise                      = [ch]
      decoder = unwords [from:' ':to | (from,to) <- sortOn fst encoding]
  in decoder ++ "\n" ++ concatMap encode input

gggMapping :: Parser (Parser Char)
gggMapping = do
  to <- anyChar <* space
  from <- takeTill isSpace
  return (string from *> pure to)

decode :: Parser String
decode = do
  gggMappings <- gggMapping `sepBy1` space <* endOfLine
  manyTill (choice gggMappings <|> anyChar) endOfInput

frequencies :: Ord a => [a] -> [(a,Int)]
frequencies xs = [(y, 1+length ys) | (y:ys) <- group (sort xs)]

type Map k v = [(k,v)]

huffman :: (Num weight, Ord weight) => [to] -> Map from weight -> Map from [to]
huffman alphabet weights = snd $ head $ until one reduce (reduce start) where
  one [_] = True
  one  _  = False
  start = [ (weight, encoding)
          | (from,weight) <- sortOn snd weights
          , let encoding = [(from,[])] ]
  reduce xs =
    let (toMerge, unTouched) = splitAt (length alphabet) xs
        (mrgWeights, mrgEncodings) = unzip toMerge
        mrgEncoding = [ (from, to:tos)
                      | (to, encoding) <- zip alphabet mrgEncodings
                      , (from, tos) <- encoding ]
        merged = (sum mrgWeights, mrgEncoding)
    in insertBy (comparing fst) merged unTouched
