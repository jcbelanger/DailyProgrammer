{- 
http://www.reddit.com/r/dailyprogrammer/comments/32goj8/20150413_challenge_210_easy_intharmonycom/
-}

import Data.Word
import Data.Bits

main = do
  [a,b] <- fmap (map read . words) getLine
  putStrLn $ challenge a b

challenge :: Word32 -> Word32 -> String
challenge a b = let matches = popCount (a `xor` complement b)
                    compat = 100 * matches `div` (bitSize a)
                in unlines [ show compat ++ "% Compatibility"
                           , show a ++ " should avoid " ++ show (complement a)
                           , show b ++ " should avoid " ++ show (complement b) ]
