{-
https://www.reddit.com/r/dailyprogrammer/comments/3xdmtw/20151218_challenge_245_hard_guess_whois/
-}

{-# OPTIONS_GHC -funbox-strict-fields -threaded -rtsopts -O2 #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Ix
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Time.Clock
import           Data.Word

newtype IP = IP4 { ipTo32 :: Word32 } deriving (Eq, Ord, Ix)

instance Show IP where
  show = intercalate "." . map show . toOctets . ipTo32

data IpBlock = IpBlock
  { begin :: !IP
  , end   :: !IP
  , name  :: !T.Text
  } deriving (Eq, Show)

instance Ord IpBlock where
  compare = comparing (rangeSize . ipRange) <> comparing begin <> comparing name

ipRange :: IpBlock -> (IP,IP)
ipRange = (,) <$> begin <*> end

fromOctets :: [Word8] -> Word32
fromOctets = foldl' (\total oct -> total `shiftL` 8 .|. fromIntegral oct) 0

toOctets :: Word32 -> [Word8]
toOctets w = [ fromIntegral (w `shiftR` 24)
             , fromIntegral (w `shiftR` 16)
             , fromIntegral (w `shiftR` 8)
             , fromIntegral w ]

parseIP4 :: Parser IP
parseIP4 = do
  a <- decimal
  char '.'
  b <- decimal
  char '.'
  c <- decimal
  char '.'
  d <- decimal
  (return . IP4 . fromOctets) [a,b,c,d]

parseBlock :: Parser IpBlock
parseBlock = IpBlock <$> parseIP4 <* char ' ' <*> parseIP4 <* char ' ' <*> takeWhile1 (notInClass "\r\n")

parseBlockFile :: Parser [IpBlock]
parseBlockFile = many1 (parseBlock <* endOfLine) <* endOfInput

parseQueryFile :: Parser [IP]
parseQueryFile = many1 (parseIP4 <* endOfLine) <* endOfInput

data Split a = Begin !a | Middle !a !a | End !a deriving (Eq, Ord, Show)

small :: Ord a => Split a -> a
small (Middle a b) = min a b
small (Begin a)    = a
small (End b)      = b

rightOf :: Split a -> Maybe a
rightOf (Middle _ x) = Just x
rightOf (Begin x)    = Just x
rightOf _            = Nothing

newtype Projection a b = Projection (M.Map a (Split b)) deriving (Eq, Ord, Show)

toProjection :: (Ord k, Ord v) => [((k,k),v)] -> Projection k v
toProjection = foldl' (\p (k,v) -> insertP k v p) (Projection M.empty)

lookupP :: (Ord a, Ord b) => a -> Projection a b -> Maybe b
lookupP key (Projection xs) = small <$> atKey <|> nearest where
  (ltKey, atKey, _) = M.splitLookup key xs
  nearest = rightOf . fst =<< M.maxView ltKey

insertP :: (Ord a, Ord b) => (a,a) -> b -> Projection a b -> Projection a b
insertP (low,hi) x (Projection xs) = Projection xs' where
    (ltLow, atLow, gtLow) = M.splitLookup low xs
    (between, atHi, gtHi) = M.splitLookup hi gtLow
    between' = M.foldlWithKey' clip xs between
    clip cur k (Begin a)    | x < a          = M.delete k cur
                            | otherwise      = M.insert k (Middle x a) cur
    clip cur k (Middle a b) | x < a && a < b = M.delete k cur
                            | x < a          = M.insert k (Middle x b) cur
                            | x < b          = M.insert k (Middle a x) cur
                            | otherwise      = cur
    clip cur k (End b)      | x < b          = M.delete k cur
                            | otherwise      = M.insert k (Middle b x) cur
    withLow = case atLow of
      Just (Begin a)    | x < a     -> M.insert low (Begin x)    between'
                        | otherwise -> between'
      Just (Middle a b) | x < b     -> M.insert low (Middle a x) between'
                        | otherwise -> between'
      Just (End b)                  -> M.insert low (Middle b x) between'
      Nothing                       -> case fst <$> M.maxView ltLow of
        Just (Begin a)    | x < a     -> M.insert low (Middle a x) between'
                          | otherwise -> between'
        Just (Middle _ b) | x < b     -> M.insert low (Middle b x) between'
                          | otherwise -> between'
        Just (End _)                  -> M.insert low (Begin x) between'
        Nothing                       -> M.insert low (Begin x) between'
    xs' = case atHi of
      Just (Begin a)                 -> M.insert hi (Middle x a) withLow
      Just (Middle a b) | x < a      -> M.insert hi (Middle x b) withLow
                        | otherwise  -> withLow
      Just (End b)      | x < b      -> M.insert hi (End x) withLow
                        | otherwise  -> withLow
      Nothing                        -> case fst <$> M.minView gtHi of
        Just (Begin _)                -> M.insert hi (End x) withLow
        Just (Middle a _) | x < a     -> M.insert hi (Middle x a) withLow
                          | otherwise -> withLow
        Just (End a)      | x < a     -> M.insert hi (Middle x a) withLow
                          | otherwise -> withLow
        Nothing                       -> M.insert hi (End x) withLow

challenge :: Projection IP IpBlock -> [IP] -> M.Map (Maybe IpBlock) Int
challenge projection = foldl' tally M.empty where
  tally totals ip = M.insertWith (+) (lookupP ip projection) 1 totals

showResults :: M.Map (Maybe IpBlock) Int -> String
showResults counts = unlines
  [ show total ++ " - " ++ maybe "<uknown>" (T.unpack . name) block
  | (block,total) <- sortOn (Down . snd) (M.assocs counts) ]

main :: IO ()
main = do
  parseStart <- getCurrentTime
  Right ips    <- parseOnly parseQueryFile <$> TIO.readFile "query10k.txt"
  Right blocks <- parseOnly parseBlockFile <$> TIO.readFile "ips300k.txt"
  parseStop <- getCurrentTime
  putStrLn $ "Parse: " ++ show (parseStop `diffUTCTime` parseStart)
  indexStart <- getCurrentTime
  let projection = toProjection [(ipRange b, b) | b <- blocks]
  indexStop <- projection `seq` getCurrentTime
  putStrLn $ "Index: " ++ show (indexStop `diffUTCTime` indexStart)
  resultsStart <- getCurrentTime
  let results = challenge projection ips
  resultsStop <- results `seq` getCurrentTime
  putStrLn $ "Results: " ++ show (resultsStop  `diffUTCTime` resultsStart)
  putStrLn $ showResults results
