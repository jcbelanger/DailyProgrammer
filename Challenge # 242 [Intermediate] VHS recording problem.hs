{-# LANGUAGE RecordWildCards #-}

import           Data.List
import           Data.Ord
import           Data.Time

data TVShow = TVShow
  { startTime :: TimeOfDay
  , endTime   :: TimeOfDay
  , name      :: String }

instance Read TVShow where
  readsPrec _ xs =
    [ (TVShow {..}, "")
    | (startTime, ' ':ys)   <- readSTimeOfDay xs
    , (endTime,   ' ':name) <- readSTimeOfDay ys ]
    where readSTimeOfDay = readSTime False defaultTimeLocale "%H%M"

overlap :: TVShow -> TVShow -> Bool
overlap a b = startTime a < endTime b && startTime b < endTime a

challenge :: TVShow -> [TVShow] -> [TVShow]
challenge mustSee = maximumBy (comparing length) . map (nubBy overlap.(mustSee:)) . subsequences

main :: IO ()
main = interact $ \input ->
  let mustSeeName:tvShowLns = lines input
      tvShows = map read tvShowLns
      Just mustSee = find ((==mustSeeName).name) tvShows
  in unlines $ map name $ challenge mustSee tvShows
