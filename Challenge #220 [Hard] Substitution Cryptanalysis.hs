{-
https://www.reddit.com/r/dailyprogrammer/comments/3b668g/20150626_challenge_220_hard_substitution/
-}

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Control.Monad

data Trie a = Trie {leaf :: Bool, follow :: Map a (Trie a) }  

instance Ord a => Monoid (Trie a) where
    mempty = Trie False Map.empty
    mappend (Trie l1 f1) (Trie l2 f2) = Trie (l1 || l2) (Map.unionWith (<>) f1 f2)

fromList :: Ord a => [a] -> Trie a
fromList = foldr (\x xs -> Trie False (Map.singleton x xs)) (Trie True Map.empty)

main = do
    dict <- foldMap fromList . lines <$> readFile "words.txt"
    interact $ \input ->
        let msg:n:knownLines = lines input
            known = Map.fromList [(x,x') | [x',x] <- knownLines]
            results = decode msg <$> sentenceEncodes dict known msg
        in unlines (catMaybes results) 

sentenceEncodes :: Trie Char -> Map Char Char -> String -> [Map Char Char]
sentenceEncodes dict known = foldM (wordEncodes dict) known . words

wordEncodes :: Trie Char -> Map Char Char -> String -> [Map Char Char]
wordEncodes dict known = map fst . filter (leaf . snd) . foldM go (known, dict) where
    go (known, pos) x
        | isAlpha x = case Map.lookup x known of
            Just x' -> case Map.lookup x' (follow pos) of 
                Just pos' -> [(known, pos')]
                Nothing   -> []
            Nothing -> [ (Map.insert x x' known, pos')
                       | (x', pos') <- Map.assocs (follow pos)
                       , Map.notMember x' known ]
        | otherwise = [(known, pos)]

decode :: String -> Map Char Char -> Maybe String
decode msg encoding = mapM go msg where 
    go x | isAlpha x = Map.lookup x encoding
         | otherwise = Just x
