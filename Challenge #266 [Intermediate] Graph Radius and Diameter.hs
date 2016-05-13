{-
https://www.reddit.com/r/dailyprogrammer/comments/4iut1x/20160511_challenge_266_intermediate_graph_radius/
-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Challenge where


import           Control.Monad
import           Control.Monad.Identity
import           Data.Array.Repa              as Repa
import           Data.Array.Repa.Eval         (Elt (..))
import           Data.Array.Repa.Shape
import           Data.Attoparsec.Text         hiding (option)
import           Data.Default                 (Default (..))
import           Data.Semigroup
import qualified Data.Text                    as T
import           Data.Text.IO                 as TIO
import qualified Data.Vector.Generic.Mutable  as GM
import qualified Data.Vector.Mutable          as M
import           Data.Vector.Unboxed          (Unbox, Vector)
import qualified Data.Vector.Unboxed          as V
import           Data.Vector.Unboxed.Deriving
import           Text.Printf


type Vertex = Int
type Weight = Int
type Graph r = Array r DIM2 (Option Weight)


main :: IO ()
main = TIO.interact $ either showError (showResults . challenge) . parseInput
  where
    showError = T.pack . ("Parse Error: " <>)
    parseInput = parseOnly (graphP <* endOfInput)
    showResults = option "No Results" $ \(Min r, Max d) ->
        T.pack $ printf "Radius: %d\nDiameter: %d" r d

graphP :: Parser (Graph U)
graphP = do
    decimal
    endOfLine
    edges <- many' (edgeP <* endOfLine)
    let n = maximum [max a b | (a,b) <- edges]
        shape = Z :. n :. n
        adjMatrix = V.create $ do
            vect <- GM.new (size shape)
            forM_ edges $ \(from,to) ->
                let ix = toIndex shape (Z :. from - 1 :. to - 1)
                in GM.write vect ix (Option $ Just 1)
            return vect
    return (Repa.fromUnboxed shape adjMatrix)

edgeP :: Parser (Vertex, Vertex)
edgeP = (,) <$> decimal <* space <*> decimal

challenge :: Graph U -> Option (Min Weight, Max Weight)
challenge g = runIdentity $ do
    let (toMax, fromMax) = (fmap Max, fmap getMax)
        toMinMax x = (Min x, Max x)
        foldMapP f = foldP (<>) mempty . Repa.map f
        foldMapAllP f = foldAllP (<>) mempty . Repa.map f
    paths <- floydWarshall g
    eccentricities <- Repa.map fromMax <$> foldMapP toMax paths
    foldMapAllP (fmap toMinMax) eccentricities

floydWarshall :: Monad m => Graph U -> m (Graph D)
floydWarshall g0 = Repa.map fromMin <$> foldM considerK g0' [0..n-1]
  where
    g0' = computeS (Repa.map toMin g0)
    (toMin, fromMin) = (fmap Min, fmap getMin)
    shape@(Z :. _ :. n) = extent g0
    considerK !g !k = computeUnboxedP (fromFunction shape considerIKJ)
      where
        considerIKJ :: DIM2 -> Option (Min Weight)
        considerIKJ (Z :. i :. j)
            | i == j    = Option Nothing
            | otherwise = old <> new
            where
                old = g ! (Z :. i :. j)
                new = do
                    ik <- g ! (Z :. i :. k)
                    kj <- g ! (Z :. k :. j)
                    return (ik + kj)

instance Elt a => Elt (Maybe a) where
    {-# INLINE touch #-}
    touch (Just x) = touch x
    touch Nothing  = return ()
    {-# INLINE zero #-}
    zero = return zero
    {-# INLINE one #-}
    one = return one

instance Elt a => Elt (Option a) where
    {-# INLINE touch #-}
    touch (Option x) = touch x
    {-# INLINE zero #-}
    zero = return zero
    {-# INLINE one #-}
    one = return one

instance Elt a => Elt (Min a) where
    {-# INLINE touch #-}
    touch (Min x) = touch x
    {-# INLINE zero #-}
    zero = return zero
    {-# INLINE one #-}
    one = return one

instance Elt a => Elt (Max a) where
    {-# INLINE touch #-}
    touch (Max x) = touch x
    {-# INLINE zero #-}
    zero = return zero
    {-# INLINE one #-}
    one = return one

instance Bounded a => Default (Min a) where
    def = minBound

instance Bounded a => Default (Max a) where
    def = maxBound

derivingUnbox "Maybe"
    [t| forall a. (Default a, Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\x -> (True, x)) |]
    [| \(b, x) -> if b then Just x else Nothing |]

derivingUnbox "Option"
    [t| forall a. (Default a, Unbox a) => Option a -> Maybe a |]
    [| getOption |]
    [| Option |]

derivingUnbox "Max"
    [t| forall a. Unbox a => Max a -> a |]
    [| getMax |]
    [| Max |]

derivingUnbox "Min"
    [t| forall a. Unbox a => Min a -> a |]
    [| getMin |]
    [| Min |]
