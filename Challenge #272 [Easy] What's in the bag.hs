{-
https://www.reddit.com/r/dailyprogrammer/comments/4oylbo/20160620_challenge_272_easy_whats_in_the_bag/
-}

import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord
import           Text.Printf

type TileCount = Map Char Int

main :: IO ()
main = interact (either showLeft showRight . challenge)

challenge :: String -> Either Char TileCount
challenge = foldrM playTile initTiles

showLeft :: Char -> String
showLeft = printf "Invalid input. More %c's have been taken from the bag than possible."

showRight :: TileCount -> String
showRight tilesLeft =
    let tilesPlayed = Map.unionWith (-) initTiles tilesLeft
    in unlines [ "Tiles Left:"
               , distribution tilesLeft
               , "Tiles Played:"
               , distribution tilesPlayed
               , "Score Left:"
               , show (score tilesLeft)
               , "Score Played:"
               , show (score tilesPlayed) ]

score :: TileCount -> Int
score = sum . Map.mapMaybeWithKey (\tile count -> (count *) <$> Map.lookup tile tilePoints)

distribution :: TileCount -> String
distribution tiles =
    unlines [ show count ++ ": " ++ intersperse ',' letters
            | byCount <- (groupBy ((==) `on` snd) . sortOn (Down . snd) . Map.toList) tiles
            , let count = snd (head byCount)
            , let letters = sort (map fst byCount) ]

playTile :: Char -> TileCount -> Either Char TileCount
playTile tile counts
    | Just count <- Map.lookup tile counts, count > 0 = Right (Map.insert tile (count - 1) counts)
    | otherwise                                       = Left tile

initTiles :: TileCount
initTiles = Map.fromList
    [ ('A', 9)
    , ('B', 2)
    , ('C', 2)
    , ('D', 4)
    , ('E',12)
    , ('F', 2)
    , ('G', 3)
    , ('H', 2)
    , ('I', 9)
    , ('J', 1)
    , ('K', 1)
    , ('L', 4)
    , ('M', 2)
    , ('N', 6)
    , ('O', 8)
    , ('P', 2)
    , ('Q', 1)
    , ('R', 6)
    , ('S', 4)
    , ('T', 6)
    , ('U', 4)
    , ('V', 2)
    , ('W', 2)
    , ('X', 1)
    , ('Y', 2)
    , ('Z', 1)
    , ('_', 2) ]

tilePoints :: Map Char Int
tilePoints = Map.fromList
    [ ('A', 1)
    , ('B', 3)
    , ('C', 3)
    , ('D', 2)
    , ('E', 1)
    , ('F', 4)
    , ('G', 2)
    , ('H', 4)
    , ('I', 1)
    , ('J', 8)
    , ('K', 5)
    , ('L', 1)
    , ('M', 3)
    , ('N', 1)
    , ('O', 1)
    , ('P', 3)
    , ('Q',10)
    , ('R', 1)
    , ('S', 1)
    , ('T', 1)
    , ('U', 1)
    , ('V', 4)
    , ('W', 4)
    , ('X', 8)
    , ('Y', 4)
    , ('Z',10)
    , ('_', 0) ]
