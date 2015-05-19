{-
http://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/
-}

import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.Ord

challenge points = 
    let (traveled, _, _) = until finish next (0, (0.5,0.5), Set.fromList points)
        finish (_, _, remain) = Set.null remain
        next (traveled, current, remain) =
            let near = F.minimumBy (comparing (dist current)) remain
            in (dist near current + traveled, near, Set.delete near remain)
    in traveled

parsePoint input = let [x,y] = map read (words input) in (x,y)

dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

main = interact $ show . challenge . map parsePoint . tail . lines
