{-|
* [2014-12-15] Challenge #193 [Easy] A Cube, Ball, Cylinder, Cone walk into a warehouse

http://www.reddit.com/r/dailyprogrammer/comments/2peac9/20141215_challenge_193_easy_a_cube_ball_cylinder/

A short and simple solution in Haskell
-}

import Control.Applicative
import Text.Printf

data Container = Cube | Sphere | Cylinder | Cone deriving (Bounded, Enum)

volume :: Container -> Double -> String
volume Cube v = printf "Cube: %.2f Width, %.2f Height, %.2f Depth" s s s
    where s = v ** (1/3)                
volume Sphere v = printf "Sphere: %.2f Radius" r
    where r = (3/4) * (v/pi) ** (1/3)
volume Cylinder v = printf "Cylinder: %.2f Tall, %.2f Diameter" h d
    where h = v/3
          d = 2*r
          r = (v / (pi * h)) ** (1/2)
volume Cone v = printf "Cone: %.2f Tall, %.2f Radius" h r
    where h = v/3
          r = (v * 3 / (h * pi)) ** (1/2)

main = do
    v <- fromIntegral . read <$> getLine
    mapM_  putStrLn $ volume <$> [minBound..maxBound] <*> pure v