{-
https://www.reddit.com/r/dailyprogrammer_ideas/comments/3vc8in/hard_guess_whois/
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import           Control.Parallel.Strategies
import           Data.Attoparsec.Text
import           Data.Ix
import           Data.List
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Word
import           Data.Bits
import           Data.Function
import           Data.Map.Strict (Map)
import qualified Data.Map                    as M
import           Data.Set (Set)
import qualified Data.Set                    as S
import           Data.Ord
import           GHC.Generics

newtype IP = IP4 { ipTo32 :: Word32 } deriving (Eq, Ord, Ix, Generic, NFData)

data Range = Range
  { begin :: !IP
  , end   :: !IP
  , name  :: !T.Text
  } deriving (Eq, Show, Generic, NFData)

instance Show IP where
  show = intercalate "." . map show . toOctets . ipTo32

instance Ord Range where
  compare = compare `on` rangeSize . ipRange

fromOctets :: [Word8] -> Word32
fromOctets = foldl' (\tot oct -> tot `shiftL` 8 .|. fromIntegral oct) 0

toOctets :: Word32 -> [Word8]
toOctets w = [ fromIntegral (w `shiftR` 24)
             , fromIntegral (w `shiftR` 16)
             , fromIntegral (w `shiftR` 8)
             , fromIntegral w ]

ipRange :: Range -> (IP,IP)
ipRange = (,) <$> begin <*> end

-- parseIPSlow :: Parser IP --needs backtracking to work!
-- parseIPSlow = IP4 . fromOctets <$> decimal `sepBy1` char '.'

parseIP :: Parser IP
parseIP = do
  a <- decimal <* char '.'
  b <- decimal <* char '.'
  c <- decimal <* char '.'
  d <- decimal
  (return . IP4 . fromOctets) [a,b,c,d]

parseRange :: Parser Range
parseRange = Range <$> parseIP <* char ' ' <*> parseIP <* char ' ' <*> takeWhile1 (notInClass "\r\n")

parseRangeFile :: Parser [Range]
parseRangeFile = many1 (parseRange <* endOfLine) <* endOfInput

parseQueryFile :: Parser [IP]
parseQueryFile = many1 (parseIP <* endOfLine) <* endOfInput

search1 :: IP -> [Range] -> Maybe Range
search1 ip = find ((`inRange` ip) . ipRange)

challenge1 :: IP -> [Range] -> String
challenge1 ip = maybe "<unknown>" (showResults ip) . search1 ip

challenge2 :: IP -> [Range] -> String
challenge2 ip ranges = case filter ((`inRange` ip) . ipRange) ranges of
    []     -> "<unknown>"
    remain -> showResults ip $ minimumBy (comparing $ rangeSize . ipRange) remain

search3 :: Set IP -> [Range] -> Map IP Range
search3 ips ranges = case break ((<S.size ips) . M.size) sieves of
  (smaller,exact:bigger) -> exact
  (smaller@(_:_),[])     -> last smaller
  _                      -> M.empty
 where
  sieves :: [Map IP Range]
  sieves = scanl' matchIPs M.empty $ sortOn (rangeSize . ipRange) ranges where
  matchIPs :: Map IP Range -> Range -> Map IP Range
  matchIPs bests rng = foldl' (logMatches rng) bests (matches ips rng) where
  logMatches :: Range -> Map IP Range -> IP -> Map IP Range
  logMatches match bests ip = M.insertWith min ip match bests --Can probably insert instead of min

matches :: Set IP -> Range -> Set IP
matches ips r = S.unions [between, a', b'] where -- largest first for efficiency
  (a,b) = ipRange r
  (_      , startMember, gt) = S.splitMember a ips
  (between, endMember  , _ ) = S.splitMember b gt
  a' = if startMember then S.singleton a else S.empty
  b' = if endMember   then S.singleton b else S.empty

challenge3 :: [IP] -> [Range] -> [String]
challenge3 ips ranges = zipWith (maybe "<unknown>" . showResults) ips pairedRanges where
  pairedRanges = map (`M.lookup` results) ips
  results = search3 (S.fromList ips) ranges

showResults :: IP -> Range -> String
showResults ip best = show ip ++ " " ++ T.unpack (name best)

main :: IO ()
main = do
  Right ranges <- parseOnly parseRangeFile <$> TIO.readFile "ips1mil.txt"
  Right ips    <- parseOnly parseQueryFile <$> TIO.readFile "query10k.txt"
  -- let r = sortOn (rangeSize . ipRange) ranges
  -- mapM_ putStrLn $ withStrategy (parBuffer 128 rdeepseq) $ map (`challenge1` r) ips
  -- mapM_ putStrLn $ withStrategy (parBuffer 128 rdeepseq) $ map (`challenge2` ranges) ips
  mapM_ putStrLn (challenge3 ips ranges)
