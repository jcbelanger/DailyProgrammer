{-
[8/08/2014] Challenge #174 [Hard] Convex Hull Problem

http://www.reddit.com/r/dailyprogrammer/comments/2cyss3/8082014_challenge_174_hard_convex_hull_problem/

I have a collection of points, called P. For this challenge the points will all be on a 2D plane. The Convex Hull problem is to find a convex polygon made from points in P which contains all of the points in P. There are several approaches to this problem, including brute-force (not good) and several O(n2) solutions (naive, not brilliant) and some fairly in-depth algorithms.
Some such algorithms are described here (a Java applet, be warned - change the display to 2d first) or on Wikipedia. The choice is yours, but because you're in /r/DailyProgrammer try and challenge yourself! Try and implement one of the more interesting algorithms.
For example, a convex hull of P:
Cannot be this because a point is excluded from the selection
Also cannot be this because the shape is not convex - the triangles enclosed in green are missing
Looks like this. The shape is convex and contains all of the points in the image - either inside it or as a boundary.
Input Description

First you will be given a number, N. This number is how many points are in our collection P.
You will then be given N further lines of input in the format:
X,Y
Where X and Y are the co-ordinates of the point on the image. Assume the points are named in alphabetical order as A, B, C, D, ... in the order that they are input.
Output Description

You must give the convex hull of the shape in the format:
ACFGKLO
Where the points are described in no particular order. (as an extra challenge, make them go in order around the shape.)
Notes

In the past we've had some very pretty images and graphs from people's solutions. If you feel up to it, add an image output from your challenge which displays the convex hull of the collection of points.
-}

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord

type Point = (Double, Double)

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan [point] = [point]
grahamScan points = let bottomLeft = minimumBy (comparing snd <> comparing fst) points
                        minAngle:byAngles = sortBy (comparing $ xAxisAngle bottomLeft) points
                        takeLeftTurns (cur:prev:rest) next = if isLeftTurn prev cur next
                                                             then next:cur:prev:rest
                                                             else next:prev:rest
                    in  foldl' takeLeftTurns [minAngle, bottomLeft] byAngles

xAxisAngle :: Point -> Point -> Double
xAxisAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1) > 0
               
parsePoint :: String -> Point
parsePoint input = let (x, ',' : y) = break (==',') input in (read x, read y)

challenge :: String -> String
challenge input = let points = map parsePoint . tail . lines $ input
                      pointMap = zip points ['A'..]
                      hull = grahamScan points
                  in  catMaybes . map (`lookup` pointMap) $ hull

main = interact challenge
