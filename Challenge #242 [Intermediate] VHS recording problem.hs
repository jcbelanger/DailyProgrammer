{-
https://www.reddit.com/r/dailyprogrammer/comments/3u6o56/20151118_challenge_242_intermediate_vhs_recording/
-}

{-# LANGUAGE RecordWildCards #-}

import           Data.List
import           Data.Ord
import           Data.Time

data TVShow = TVShow
  { start :: TimeOfDay
  , end   :: TimeOfDay
  , name  :: String }

instance Read TVShow where
  readsPrec _ xs =
    [ (TVShow {..}, "")
    | (start, ' ':ys)   <- readSTimeOfDay xs
    , (end,   ' ':name) <- readSTimeOfDay ys ]
    where readSTimeOfDay = readSTime False defaultTimeLocale "%H%M"

overlap :: TVShow -> TVShow -> Bool
overlap a b = start a < end b && start b < end a

challenge :: TVShow -> [TVShow] -> [TVShow]
challenge must = maximumBy (comparing length) . map (nubBy overlap . (must:)) . permutations

main :: IO ()
main = interact $ \input ->
  let mustName:tvShowLns = lines input
      tvShows = map read tvShowLns
      Just must = find ((==mustName).name) tvShows
  in unlines $ map name $ challenge must tvShows
