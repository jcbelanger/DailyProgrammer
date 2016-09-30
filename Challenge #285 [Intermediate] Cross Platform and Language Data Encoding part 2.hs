{-
https://www.reddit.com/r/dailyprogrammer/comments/54wihd/20160928_challenge_285_intermediate_cross/
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Binary
import qualified Data.ByteString.Lazy as BS
import           Data.Time
import           GHC.Generics

newtype FixedWidthDate = FixedWidthDate Day deriving (Enum, Eq, Ord, Read, Show, ParseTime, FormatTime)

instance Binary FixedWidthDate where
    put (FixedWidthDate date) =
        let (year, month, day) = toGregorian date
            bytes = [ fromInteger (year - 1900)
                    , fromIntegral month
                    , fromIntegral day ]
        in mapM_ putWord8 bytes
    get = do
        year  <- fromIntegral <$> getWord8
        month <- fromIntegral <$> getWord8
        day   <- fromIntegral <$> getWord8
        let date = fromGregorian (year + 1900) month day
        return (FixedWidthDate date)

data Record = Record
    { firstName   :: String
    , lastName    :: String
    , dateOfBirth :: FixedWidthDate
    } deriving (Eq, Ord, Show, Generic)

instance Binary Record

parseDate :: String -> Maybe FixedWidthDate
parseDate = parseTimeM True defaultTimeLocale "%Y/%-m/%-d"

main :: IO ()
main = do
    Just dates <- mapM parseDate . lines <$> getContents
    BS.putStr (encode dates)
