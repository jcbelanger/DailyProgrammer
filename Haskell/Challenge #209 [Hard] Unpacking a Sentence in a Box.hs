{-# LANGUAGE TupleSections #-}

-- http://www.reddit.com/r/dailyprogrammer/comments/322hh0/20150410_challenge_209_hard_unpacking_a_sentence/

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String
import Control.Arrow
import Control.Applicative
import Control.Monad.List
import Control.Monad.Par
import System.IO

data Trie a = Trie
    { leaf   :: Any
    , follow :: M.Map a (Trie a)
    } deriving (Eq, Show)

isLeaf :: Trie a -> Bool
isLeaf = getAny . leaf

instance Ord a => Monoid (Trie a) where
    mempty = Trie mempty mempty
    mappend (Trie l1 f1) (Trie l2 f2) = Trie (l1 <> l2) (M.unionWith (<>) f1 f2)

fromList :: Ord a => [a] -> Trie a
fromList = foldr consTrie $ Trie (Any True) M.empty where
    consTrie x xs = Trie (Any False) (M.singleton x xs)

toTrie :: Ord a => [[a]] -> Trie a
toTrie = F.foldMap fromList

challenge :: Trie Char -> M.Map (Int, Int) Char -> (Int, Int) -> [String]
challenge lang posValMap startPos =
    let Just startVal = M.lookup startPos posValMap
        size = fromIntegral $ M.size posValMap
        maxdepth = if size <= 100 then 0 else floor $ logBase 2 size
        start = (startPos, startVal, posValMap, lang, id)
        finished (pos, val, remain, trie, sentence) = do
            guard $ M.size remain == 1
            val <- M.lookup pos remain
            finalTrie <- M.lookup val (follow trie)
            guard $ isLeaf finalTrie
            return $ sentence [val]
        refine (pos@(x, y), val, remain, trie, sentence) =
            [ (pos', val', remain', trie', sentence')
            | let remain' = M.delete pos remain
            , pos' <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
            , val' <- maybeToList $ M.lookup pos' remain'
            , let continueWord = (sentence.(val:),        ) <$> M.lookup val (follow trie)
            , let startNewWord = (sentence.(' ':).(val:), ) <$> M.lookup val (follow lang) 
            , (sentence', trie') <- catMaybes [continueWord, guard (isLeaf trie) >> startNewWord] ]
    in parsearch maxdepth finished refine start

main = do
    let canonical = map toUpper . filter isAlpha
    lang <- toTrie . map canonical . lines <$> readFile "enable1.txt"
    interact $ \input ->
        let meta:rows = lines input
            [size, x, y] = map read $ words meta
            posMap = M.fromList [ ((row, col), char)
                                | (row, line)   <- zip [1..] rows
                                , (col, char:_) <- zip [1..] (map canonical $ words line) ]
        in unlines $ challenge lang posMap (x, y)

search :: (partial -> Maybe solution)   -- finished?
       -> (partial -> [partial])        -- refine a solution
       -> partial                       -- initial solution
       -> [solution]
search finished refine start = generate start where
    generate partial
       | Just soln <- finished partial = [soln]
       | otherwise  = concatMap generate (refine partial)
       
parsearch :: NFData solution
          => Int                           -- spawn threads upto depth
          -> (partial -> Maybe solution)   -- finished?
          -> (partial -> [partial])        -- refine a solution
          -> partial                       -- initial solution
          -> [solution]
parsearch maxdepth finished refine emptysoln
  = runPar $ generate 0 emptysoln
  where
    generate d partial | d >= maxdepth 
       = return (search finished refine partial)
    generate d partial
       | Just soln <- finished partial = return [soln]
       | otherwise  = do
           solnss <- parMapM (generate (d+1)) (refine partial)
           return (concat solnss)
