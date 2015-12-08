{-
https://www.reddit.com/r/dailyprogrammer/comments/3vswuc/20151207_challenge_244_intermediate_turn_any/
-}

{-# LANGUAGE TypeOperators, FlexibleContexts, TypeSynonymInstances, OverloadedLists  #-}
{-# LANGUAGE FlexibleInstances, LiberalTypeSynonyms, RankNTypes, TypeFamilies #-}

import Data.Array.Repa as R
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Base
import GHC.Exts

type JScalar a = Array D DIM0 a
type JList   a = Array D DIM1 a
type JTable  a = Array D DIM2 a
type JBrick  a = Array D DIM3 a

type JVerb shx shy shz ax ay az = JVerbFull D D shx shy shz ax ay az
type JVerbFull rx ry shx shy shz ax ay az
  = (Shape shx, Shape shy, Shape shz, Source rx shx, Source ry shy)
  => Maybe (Array rx shx ax) -> Array ry shy ay -> Array D shz az

instance Unbox a => IsList (JList a) where
  type Item (JList a) = a
  fromList = jList

instance Num a => Num (JScalar a) where
  fromInteger = jScalar . fromInteger

jScalar :: a -> JScalar a
jScalar = fromFunction Z . const

jList :: Unbox a => [a] -> JList a
jList xs = delay $ fromListUnboxed (Z :. length xs) xs

iota :: Integral x => JVerb DIM0 DIM1 shz x x x
iota x y = R.fromFunction shape ((+ offset) . fromIntegral . R.toIndex shape) where
  shape = R.shapeOfList (fromIntegral <$> R.toList y)
  offset = fromIntegral $ maybe 0 (! Z) x

add :: Num a => JVerb shx shy shy a a a
add = liftJ00 (jScalar 0) (+)

liftJ00 :: Array D shx0 x -> (x -> y -> z) -> JVerb shx shy shy x y z
liftJ00 x0 f (Just x) y
  | rx == ry  = R.zipWith f (reshape shy x) y --TODO fill zeros
  | otherwise = undefined
 where
  (shx, shy) = (extent x , extent y)
  (rx, ry) = (rank shx, rank shy)

main :: IO ()
main = do
  print =<< computeUnboxedP iota1
  print =<< computeUnboxedP iota2
  print =<< computeUnboxedP iota3
  print =<< computeUnboxedP iota4
  print =<< computeUnboxedP iota5
  print =<< computeUnboxedP add1

add1 :: JList Int
add1 = Just (jList [1,2,3]) `add` jList [1,2,3]

iota1 :: JList Int
iota1 = Nothing `iota` [4]

iota2 :: JTable Int
iota2 = Nothing `iota` [2,3]

iota3 :: JBrick Int
iota3 = Nothing `iota` [1,2,3]

iota4 :: JList Int
iota4 = Just 1 `iota` [4]

iota5 :: JTable Int
iota5 = Just 10 `iota` [2,3]
