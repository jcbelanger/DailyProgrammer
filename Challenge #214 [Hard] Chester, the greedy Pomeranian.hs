{-
http://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/
-}

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Parallel.Strategies
import Control.Arrow

data Sheet = Sheet
    { color  :: Int
    , x      :: Int
    , y      :: Int
    , width  :: Int
    , height :: Int }

main = interact $ \input ->
    let sizeLine:sheetLines = lines input
        [canvasW, canvasH] = map read (words sizeLine)
        sheets = foldl' (\xs x -> parseSheet x : xs) [] sheetLines
        canvas = [ (a,b) | a <- [0 .. canvasW - 1], b <- [0 .. canvasH - 1] ]
        colors = withStrategy (parBuffer 64 rdeepseq) $ map (colorAt sheets) canvas
        count = foldr (flip (Map.insertWith (+)) 1) Map.empty colors
    in unlines $ map (\(c, n) -> unwords [show c, show n]) (Map.toAscList count)

parseSheet input = let [c, x, y, w, h] = map read (words input) in Sheet c x y w h

colorAt sheets point | Just sheet <- find (contains point) sheets = color sheet
                     | otherwise                                  = 0

contains (px,py) (Sheet c x y w h) = x <= px && px < x + w && y <= py && py < y + h
