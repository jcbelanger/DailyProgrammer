{-
https://www.reddit.com/r/dailyprogrammer/comments/3zqiiq/20160106_challenge_248_intermediate_a_measure_of/
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

import Control.Applicative
import Control.Monad
import Data.Array.Repa                     as Repa
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Attoparsec.ByteString          as Atto
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                     as BS
import Data.ByteString.Char8               as BSC8
import Data.Word

import Control.DeepSeq
import Data.Time.Clock

type Color = (Word8,Word8,Word8)

main :: IO ()
main = BSC8.interact $ either error id . (toPpmP3 <=< sobel <=< parseOnly ppm)

ppm :: Parser (Array U DIM2 Color)
ppm = choice [ppmP3,ppmP6]

toPpmP3 :: Monad m => Array U DIM2 Color -> m ByteString
toPpmP3 pixels = do
    bytes <- toPpmBytes pixels
    let format = "P3"
        (Z :. rows :. cols) = extent pixels
        dim = BSC8.unwords (BSC8.pack . show <$> [cols, rows])
        maxVal = "255"
        body = BSC8.unwords (BSC8.pack . show <$> toList bytes)
    return $ BSC8.unwords [format,dim,maxVal,body,"\n"]

ppmP6 :: Parser (Array U DIM2 Color)
ppmP6 = do
    many ppmWhiteSpace
    string "P6"
    some ppmWhiteSpace
    cols <- decimal
    some ppmWhiteSpace
    rows <- decimal
    some ppmWhiteSpace
    string "255" --only support word8 atm
    some ppmWhiteSpace
    bytes <- Atto.takeTill isEndOfLine
    fromPpmBytes rows cols bytes

ppmP3 :: Parser (Array U DIM2 Color)
ppmP3 = do
    many ppmWhiteSpace
    string "P3"
    some ppmWhiteSpace
    cols <- decimal
    some ppmWhiteSpace
    rows <- decimal
    some ppmWhiteSpace
    string "255" --only support word8 atm
    some ppmWhiteSpace
    bytes <- decimal `sepBy` some ppmWhiteSpace
    many ppmWhiteSpace
    fromPpmBytes rows cols (BS.pack bytes)

ppmWhiteSpace :: Parser ()
ppmWhiteSpace = void (Atto.takeWhile1 isSpace_w8 <|> ppmComment)

ppmComment :: Parser ByteString
ppmComment = char '#' *> Atto.takeWhile (not . isEndOfLine) <* optional endOfLine

fromPpmBytes ::    Monad m => Int -> Int -> ByteString -> m (Array U DIM2 Color)
fromPpmBytes rows cols bytes =
    let byteArr = fromByteString (Z :. rows :. 3*cols) bytes

        toColor source (Z :. row :. col) = (r,g,b) where
            [r,g,b] = [source (Z :. row :. 3*col + offset) | offset <- [0,1,2]]

    in computeUnboxedP $ unsafeTraverse byteArr (const $ Z :. rows :. cols) toColor

toPpmBytes :: Monad m => Array U DIM2 Color -> m (Array U DIM2 Word8)
toPpmBytes colors =
    let (Z :. rows :. cols) = extent colors

        toByte source (Z :. row :. col) = [r,g,b] !! offset
            where (col',offset) = col `quotRem` 3
                  (r,g,b) = source (Z :. row :. col')

    in computeUnboxedP $ unsafeTraverse colors (const $ Z :. rows :. 3*cols) toByte

toGray :: Color -> Double
toGray (r8,g8,b8) = 0.2126*r + 0.7152*g + 0.0722*b
    where [r,g,b] = fromIntegral <$> [r8,g8,b8]

fromGray :: Double -> Color
fromGray y = let x = round y in (x,x,x)

sobel :: Monad m => Array U DIM2 Color -> m (Array U DIM2 Color)
sobel img = do
    let hCoeff = [stencil2| -1  0  1
                            -2  0  2
                            -1  0  1 |]
        vCoeff = [stencil2| -1 -2 -1
                             0  0  0
                             1  2  1 |]
    grey <- computeUnboxedP $ Repa.map toGray img
    horz2 <- computeUnboxedP $ Repa.map (^2) $ mapStencil2 BoundClamp hCoeff grey
    vert2 <- computeUnboxedP $ Repa.map (^2) $ mapStencil2 BoundClamp vCoeff grey
    computeUnboxedP $ Repa.map (fromGray . sqrt) (Repa.zipWith (+) horz2 vert2)
