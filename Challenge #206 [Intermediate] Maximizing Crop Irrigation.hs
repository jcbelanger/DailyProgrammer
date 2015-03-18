{-
http://www.reddit.com/r/dailyprogrammer/comments/2zezvf/20150318_challenge_206_intermediate_maximizing/

[2015-03-18] Challenge #206 [Intermediate] Maximizing Crop Irrigation
submitted 3 hours ago by jnazario2 0
Description

You run a farm which isn't doing so well. Your crops that you planted aren't coming up, and your bills are bigger than your expected proceeds. So, you have to conserve water and focus instead on the plants that are growing. You have a center pivot watering system which has a rotating sprinkler around a central pivot, creating a circular watered area. For this challenge, you just have to decide where to locate it based on this year's crops.
Some notes:
Because this is a simple grid, we're only dealing with integers in this challenge.
For partially covered squares, round down: the sprinkler covers the crop if the distance from the sprinkler is less than or equal to the sprinklers radius.
If you place the sprinkler on a square with a crop, you destroy the crop so handle accordingly (e.g. deduct 1 from the calculation).
If in the event you find two or more placements that yield identical scores, pick any one of them (or even emit them all if you so choose), this is entirely possible.
Input Description

You'll be given three integers (h w r) which correspond to the number of rows (h) and columns (w) for the ASCII map (respectively) and then the radius (r) of the watering sprinkler. The ASCII map will have a "." for no crop planted and an "x" for a growing crop.
Output Description

You should emit the coordinates (0-indexed) of the row and column showing where to place the center of the sprinkler. Your coordinates should be integers.
Challenge Input

51 91 9
......x...x....x............x............x.................................x...............
.........x...........x...................x.....x...........xx.............x................
...........x.................x.x............x..........................x................x..
......x...x.....................x.....x....x.........x......x.......x...x..................
.x...x.....x................xx...........................x.....xx.....x............x.......
.....xx.......x..x........x.............xx........x.......x.....................x.......x..
...x..x.x..x......x..............................................................x...x.....
........x..x......x......x...x.x....x.......x........x..x...........x.x...x..........xx....
...............x.x....x...........x......x.............x..........................x........
...................x........x..............................................................
..x.x.....................................x..x.x......x......x.............................
......x.............................................................................x..x...
......x....x...............x...............................................................
............x.............x.............................x...............x................x.
..xx........xx............x...x......................x.....................................
........x........xx..............x.....................x.x.......x........................x
.......x....................xx.............................................................
............x...x.........x...xx...............x...........................................
.............................x...............xx..x...........x....x........x...x.......x.x.
..........x.......................x.....................................x..................
...xx..x.x..................x........................x.....................x..x.......x....
.............xx..........x...............x......................x.........x.........x....x.
...............................x.....................x.x...................................
...................x....x............................x...x.......x.............x....x.x....
.x.xx........................x...................................x.....x.......xx..........
.......x...................................................................................
.........x.....x.................x.................x...x.......x..................x........
.......x................x.x...................................x...xx....x.....x...x........
..............................................x..................x.........................
............................x........x.......x............................................x
..x.............x.....x...............x............x...x....x...x..........................
.......................xx.................x...................x...................x.......x
.x.x.............x....x.................................x...........x..x..........x.....x..
...x..x..x......................x...........x..........x.............xxx....x..........x...
...........................................................x...............................
x......x.....x................x...............x....................................x.......
..x...........................x............x..........x....x..............................x
.......................x.......xx...............x...x.x.................x..x............x..
x................x.......x........x.............................x.x.x...................x.x
.......................x...x.......................................................x.......
.x..................x.....x..........................................x...........x.........
.x...................x........x.................x..........xx..................x..x........
.x..........x...x...........................x.x....................x..x.......x............
.............x...x..................x................x..x.x.....xxx..x...xx..x.............
.x...................x.x....x...x.................x.............................x.....x....
......................x.x........x...........x...................................x......x..
................x....................................x....x....x......x..............x..x..
......x.........................................x..x......x.x.......x......................
.x..............................x..........x.x....x.................x......................
x..x...........x..x.x...x..........................................x..............xx.......
..xx......x.......x.x.................x......................................x.............
Bonus

Emit the map with the circle your program calculated drawn.
Credit

This challenge was inspired by a question on IRC from user whatiswronghere.
Notes

Have a cool idea for a challenge? Submit it to /r/DailyProgrammer_Ideas!
-}

import Control.Applicative
import Data.List
import Data.Ord

main = interact $ \input ->
    let meta:field2d = lines input
        [row, col, radius] = map read (words meta)   
        field = toIndexList field2d
        crops = map fst $ filter ((=='x').snd) field
        sprinklers = (,) <$> [0..row-1] <*> [0..col-1]
        best = maximumBy (comparing $ watered radius crops) sprinklers
        in  unlines [ "Position: " ++ show best
                    , "Watered: " ++ (show $ watered radius crops best)
                    , "Field:"
                    , showField field row col radius best ]

watered :: Int -> [(Int,Int)] -> (Int, Int) -> Int
watered radius crops sprinkler = 
    length . filter ((<=radius).(dist sprinkler)) . filter (/=sprinkler) $ crops
    
showField field row col radius sprinkler = 
    unlines . take row . chunks col $ map showSpace field
    where
        showSpace (pos, c) | pos == sprinkler             = 'O'
                           | c == 'x'                     = 'x'
                           | dist pos sprinkler <= radius = '~'
                           | otherwise                    = '.'

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1,y1) (x2,y2) = floor . sqrt . fromIntegral $ (x1-x2)^2 + (y1-y2)^2
    
toIndexList :: [[a]] -> [((Int, Int), a)]
toIndexList = concatMap flatten . index . map index
    where index = zip [0..]
          flatten (i, row) = map (\(j, val) -> ((i,j),val)) row
                       
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs
              in  ys : chunks n zs
