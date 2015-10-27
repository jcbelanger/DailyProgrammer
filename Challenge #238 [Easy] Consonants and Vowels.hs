{-
https://www.reddit.com/r/dailyprogrammer/comments/3q9vpn/20151026_challenge_238_easy_consonants_and_vowels/
-}

import           Control.Monad.Random
import           Data.Char
import           Data.List
import           Data.Maybe

vowels, consonants :: [Char]
vowels = "aeiou"
consonants = ['a'..'z'] \\ vowels

toLetter :: MonadRandom m => Char -> m (Maybe Char)
toLetter 'v' = Just <$> randFrom vowels
toLetter 'c' = Just <$> randFrom consonants
toLetter 'V' = Just <$> randFrom (map toUpper vowels)
toLetter 'C' = Just <$> randFrom (map toUpper consonants)
toLetter  _  = return Nothing

randFrom :: MonadRandom m => [a] -> m a
randFrom avail = (avail !!) <$> getRandomR (0, length avail - 1)

challenge :: RandomGen g => g -> String -> String
challenge gen input = fromMaybe "Invalid Input" (sequence letters)
  where letters = evalRand (mapM toLetter input) gen

main :: IO ()
main = interact . challenge =<< getStdGen
