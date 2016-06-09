{-
https://www.reddit.com/r/dailyprogrammer/comments/4n6hc2/20160608_challenge_270_intermediate_generating/
-}

{-# LANGUAGE FlexibleContexts #-}

module Challenge where

import           Control.Monad.Loops
import           Control.Monad.Random hiding (fromList)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.MultiSet              (MultiSet)
import qualified Data.MultiSet              as MultiSet
import           System.Random

newtype MarkovBigram a = MarkovBigram
    { entries :: Map (a,a) (MultiSet a)
    } deriving (Eq, Ord, Show)

instance Ord a => Monoid (MarkovBigram a) where
    mempty = MarkovBigram Map.empty
    mappend (MarkovBigram a) (MarkovBigram b) = MarkovBigram (Map.unionWith (<>) a b)

singleton :: a -> a -> a -> MarkovBigram a
singleton a b c = MarkovBigram (Map.singleton (a,b) (MultiSet.singleton c))

fromList :: Ord a => [a] -> MarkovBigram a
fromList xs = mconcat [singleton a b c | a:b:c:_ <- tails xs]

markov :: (MonadRandom m, Ord a) => MarkovBigram a -> m (Maybe [a])
markov = runReaderT $ runMaybeT $ do
    follows <- asks entries
    (guard . not . Map.null) follows
    ix <- getRandomR (0, Map.size follows - 1)
    let (a,b) = fst (Map.elemAt ix follows)
    xs <- evalStateT (unfoldM step) (a,b)
    return (a:b:xs)

step :: (MonadState (a,a) m, MonadReader (MarkovBigram a) m, MonadRandom m, Ord a) => m (Maybe a)
step = runMaybeT $ do
    (a,b) <- get
    follow <- MaybeT $ asks (Map.lookup (a,b) . entries)
    let add count occur = let count' = count + occur in (count', count')
        (total, accums) = Map.mapAccum add 0 (MultiSet.toMap follow)
    r <- getRandomR (0, total)
    (c,_) <- MaybeT . return $ find ((>=r) . snd) (Map.toList accums)
    put (b,c)
    return c

main :: IO ()
main = putStrLn =<< challenge =<< getContents

challenge :: MonadRandom m => String -> m String
challenge = fmap (maybe "Not enough words" unwords) . markov . fromList . words
