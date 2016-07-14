{-
https://www.reddit.com/r/dailyprogrammer/comments/4so25w/20160713_challenge_275_intermediate_splurthian/
-}

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as Map

type Element = String
type Symbol = String

splurth :: Element -> [Symbol]
splurth element = [[toUpper x, toLower y] | x:xs <- tails element, y <- xs]

challenge :: [Element] -> Either Element (Map Symbol Element)
challenge = foldlM insertSymbol Map.empty
  where
    insertSymbol :: Map Symbol Element -> Element -> Either Element (Map Symbol Element)
    insertSymbol prev element
        | Just symbol <- find (`Map.notMember` prev) (splurth element) = Right (Map.insert symbol element prev)
        | otherwise                                                    = Left element

toMaybe :: Either a b -> Maybe b
toMaybe (Right x) = Just x
toMaybe _         = Nothing

bonus :: [Element] -> Maybe [Element]
bonus elements = asum [perm <$ toMaybe (challenge perm)| perm <- permutations elements]
