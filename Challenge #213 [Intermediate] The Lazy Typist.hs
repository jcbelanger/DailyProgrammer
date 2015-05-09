{-
http://www.reddit.com/r/dailyprogrammer/comments/351b0o/20150506_challenge_213_intermediate_the_lazy/
-}

import Data.Char
import Data.List
import Data.Function
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import Control.Applicative
import Control.Arrow
import Control.Monad.RWS.Strict
import Text.Printf
import Control.Parallel.Strategies

type Pos = (Int, Int)
type Hand = Maybe Pos
type Keyboard = Map Char [Pos]
type App a = RWST Keyboard (Sum Int, [Action]) (Hand, Hand) [] a
data Action = Use String Pos | Move String Pos Pos

manhattanDist :: Pos -> Pos -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

keyboard = [ "qwertyuiop"
           , "asdfghjkl "
           , "^zxcvbnm ^"
           , "   #####  " ]

keyPos = [ (canonical key, (x,y))
         | (y, row) <- zip [0..] keyboard
         , (x, key) <- zip [0..] row
         , key /= ' ' ]

canonical '#' = ' '
canonical  k  =  k

keyMap :: Keyboard
keyMap = let groupedKeys = groupBy ((==) `on` fst) $ sortBy (comparing fst) keyPos
         in  Map.fromList $ map (fst.head &&& map snd) groupedKeys


main = interact $ \input ->
    case map snd $ evalRWST (typeSentence input) keyMap (Nothing, Nothing) of
        []    -> "keyboard does not support letters in input"
        solns -> let parSolns = withStrategy (parBuffer 32 parFst) solns
                     parFst (a,b) = (,) <$> rpar a <*> rseq b
                     (Sum tot, steps) = minimumBy (comparing fst) parSolns
                 in unlines $ map showAction steps ++ [printf "Total effort: %d" tot]

--Same as typeKey, but always starts with left to half the search space
typeSentence :: String -> App [()]
--typeSentence [] = return ()
typeSentence (firstKey:rest) = do
    keyMap <- ask
    keyStroke <- lift $ do
        poss <- catMaybes [Map.lookup (toLower firstKey) keyMap]
        pos <- poss
        if isUpper firstKey
        then [leftHandTo shift >> rightHandTo pos 
             | shifts <- catMaybes [Map.lookup '^' keyMap]
             , shift <- shifts ]
        else [leftHandTo pos]
    keyStroke
    mapM typeKey rest

typeKey :: Char -> App ()
typeKey key = do
    keyMap <- ask
    keyStroke <- lift $ do
        poss <- catMaybes [Map.lookup (toLower key) keyMap]
        pos <- poss
        if isUpper key
        then [ shiftStroke 
             | shifts <- catMaybes [Map.lookup '^' keyMap]
             , shift <- shifts
             , shiftStroke <- [ leftHandTo  shift >> rightHandTo pos
                              , rightHandTo shift >> leftHandTo  pos ]]
        else [leftHandTo pos, rightHandTo pos]
    keyStroke

leftHandTo :: Pos -> App ()
leftHandTo new = do
    (left, right) <- get
    put (Just new, right)
    logMove "left" left new

rightHandTo :: Pos -> App ()
rightHandTo new = do
    (left, right) <- get
    put (left, Just new)
    logMove "right" right new

logMove :: String -> Hand -> Pos -> App ()
logMove which hand new = do
    let (effort, action) = case hand of
            Nothing  -> (0, Use which new)
            Just old -> (manhattanDist old new, Move which old new)
    tell (Sum effort, [action])
    
    
showAction (Use which new) = printf "%s: Use %s hand" (keyAt new) which
showAction (Move which old new) = printf "%s: Move %s hand from %s (effort: %d)"
    (keyAt new) which (keyAt old) (manhattanDist old new)

showKey ' ' = "Space"
showKey '^' = "Shift"
showKey  k  = [toUpper k]

keyAt pos | Just key <- lookup pos $ map swap keyPos = showKey key
          | otherwise = "Key not found"
