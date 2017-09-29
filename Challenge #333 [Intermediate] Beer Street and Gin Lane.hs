{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

import           Control.Applicative
import qualified Control.Foldl                    as F
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as B8
import           Data.Char
import qualified Data.Csv                         as Csv
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict              as HM
import           Data.List.Split
import           Data.Monoid
import           Data.Ord
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           GHC.Generics
import           Pipes
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Csv                        as PC
import qualified Pipes.Prelude                    as P
import qualified System.IO                        as IO


data Invoice = Invoice
    { invoiceItemNumber :: !Text
    , date              :: !DayMMDDYY
    , storeNumber       :: !Int
    , storeName         :: !Text
    , address           :: !Text
    , city              :: !Text
    , zipCode           :: !Text
    , storeLocation     :: !Text
    , countyNumber      :: !Int
    , county            :: !Text
    , category          :: !(Maybe Int)
    , categoryName      :: !(Maybe Text)
    , vendorNumber      :: !Int
    , vendorName        :: !Text
    , itemNumber        :: !Int
    , itemDescription   :: !Text
    , pack              :: !Int
    , bottleVolumeML    :: !Double
    , stateBottleCost   :: !USD
    , stateBottleRetail :: !USD
    , bottlesSold       :: !Int
    , saleDollars       :: !USD
    , volumeSoldLiters  :: !Double
    , volumeSoldGallons :: !Double
    } deriving (Eq, Ord, Show, Generic)

toCsvField :: String -> String
toCsvField "invoiceItemNumber" = "Invoice/Item Number"
toCsvField "bottleVolumeML"    = "Bottle Volume (ml)"
toCsvField "saleDollars"       = "Sale (Dollars)"
toCsvField "volumeSoldLiters"  = "Volume Sold (Liters)"
toCsvField "volumeSoldGallons" = "Volume Sold (Gallons)"
toCsvField x                   = fromCamelCase x

fromCamelCase :: String -> String
fromCamelCase (x:xs) = toUpper x:unwords ((split $ keepDelimsL $ whenElt isUpper) xs)
fromCamelCase [] = []

invoiceOptions :: Csv.Options
invoiceOptions = Csv.defaultOptions { Csv.fieldLabelModifier = toCsvField }

instance Csv.FromNamedRecord Invoice where
  parseNamedRecord = Csv.genericParseNamedRecord invoiceOptions

instance Csv.DefaultOrdered Invoice where
  headerOrder = Csv.genericHeaderOrder invoiceOptions

newtype USD = USD { usdValue :: Double } deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

instance Show USD where
  show = ('$':) . show . usdValue

parseUSD :: A8.Parser USD
parseUSD = USD <$ A8.char '$' <*> A8.double

instance Csv.FromField USD where
  parseField bytes = case A8.parseOnly parseUSD bytes of
      (Right usd) -> pure usd
      (Left _)    -> empty

newtype DayMMDDYY = DayMMDDYY { toDay :: Day } deriving (Eq, Ord, Show, ParseTime)

instance Csv.FromField DayMMDDYY where
  parseField = parseTimeM False defaultTimeLocale "%-m/%-d/%Y" . B8.unpack

main :: IO ()
main = IO.withFile "Iowa_Liquor_Sales.csv" IO.ReadMode $ \h -> do
    let invoices = parseInvoice (PB.fromHandle h)
        questions = (,,,) <$> question1 <*> question2 <*> question3 <*> question4
    answers <- F.purely P.fold questions invoices
    print answers

parseInvoice :: Monad m => Producer BS.ByteString m () -> Producer Invoice m ()
parseInvoice source = PC.decodeByName source >-> P.concat

data Entry k v = Entry
    { key :: k
    , val :: v
    } deriving (Show)

instance Hashable k => Hashable (Entry k v) where
    hashWithSalt s = hashWithSalt s . key

instance Eq k => Eq (Entry k v) where
    (==) = (==) `on` key

groupOn
    :: (Eq key, Hashable key, Monoid val)
    => (a -> key)
    -> (a -> val)
    -> F.Fold (key, val) b
    -> F.Fold a b
groupOn key val sumary = F.Fold step mempty extract
  where
    step hm x = HM.insertWith (<>) (key x) (val x) hm
    extract = F.fold sumary . HM.mapWithKey (,)

--What's the most popular non-beer beverage bought in 2016?
question1 :: F.Fold Invoice (Maybe (Entry Int Text, Sum Int))
question1 = nonBeer2016 $ groupOn item (const 1) mostPopular
  where
    item = Entry <$> itemNumber <*> itemDescription
    nonBeer = (Just "HIGH PROOF BEER - AMERICAN" /=) . categoryName
    year day = let (y, _, _) = toGregorian day in y
    sold2016 = (2016==) . year . toDay . date
    nonBeer2016 = F.handles $ F.filtered (liftA2 (&&) sold2016 nonBeer)
    mostPopular = F.maximumBy (comparing snd)

--What store has made the most profit (the difference between the state cost per bottle and the sales price per bottle times the quantity of all bottles sold)?
question2 :: F.Fold Invoice (Maybe (Entry Int Text, Sum USD))
question2 = groupOn store (Sum . profit) (F.maximumBy (comparing snd))
  where
    store = Entry <$> storeNumber <*> storeName
    profit Invoice{..} = fromIntegral bottlesSold * (stateBottleRetail - stateBottleCost)

--What day of the week sees the most vodka sales? (0 = Sunday, 1 = Monday, ..)
question3 :: F.Fold Invoice (Maybe (Int, Sum USD))
question3 = vodka $ groupOn dayOfWeek (Sum . saleDollars) (F.maximumBy (comparing snd))
  where
    isVodka = T.isInfixOf "VODKA" . T.toUpper . itemDescription
    vodka = F.handles (F.filtered isVodka)
    dayOfWeek = snd . sundayStartWeek . toDay . date

--Question 4: Where in the world is all of that root beer schnapps going?
question4 :: F.Fold Invoice (HM.HashMap (Entry Int Text) (Sum Double))
question4 = rootSchps $ groupOn store (Sum . volumeSoldLiters) F.hashMap
  where
    isRootSchp = T.isInfixOf "Rootbeer Schnapps" . itemDescription
    rootSchps = F.handles (F.filtered isRootSchp)
    store = Entry <$> storeNumber <*> storeName
