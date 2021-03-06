{-
https://www.reddit.com/r/dailyprogrammer/comments/3gpjn3/20150812_challenge_227_intermediate_contiguous/
-}

import Data.Graph

main = interact challenge

challenge input | (graph,_,_) <- graphFromEdges 
    [ ((), (x,y), [(x+1,y),(x-1,y),(x,y+1),(x,y-1)])
    | (y, line) <- zip [1..] (lines input)
    , (x, 'x')  <- zip [1..] line ]
    = show . length $ components graph
