{-
https://www.reddit.com/r/dailyprogrammer/comments/3zqiiq/20160106_challenge_248_intermediate_a_measure_of/
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

import Control.Monad
import Control.Applicative
import Data.Array.Repa                     as Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Operators.Traversal
import Data.Attoparsec.ByteString          as Atto
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                     as BS
import Data.ByteString.Char8               as BSC8
import Data.Word

type Color = (Word8,Word8,Word8)

main :: IO ()
main = do
  Right img <- parseOnly ppmP3 <$> BS.getContents
  img' <- computeUnboxedP $ toPpmBytes (sobel img)
  BSC8.putStrLn (toPpmP3 img')

toPpmP3 :: Source r Word8 => Array r DIM2 Word8 -> ByteString
toPpmP3 pixels = BSC8.unwords [format,dim,maxVal,bytes,"\n"]
  where
    format = "P3"
    (Z :. rows :. cols) = extent pixels
    dim = BSC8.unwords (BSC8.pack . show <$> [cols `quot` 3, rows])
    maxVal = "255"
    bytes = BSC8.unwords (BSC8.pack . show <$> toList pixels)

ppmP3 :: Parser (Array D DIM2 Color)
ppmP3 = do
  many ppmWhiteSpace
  string "P3"
  some ppmWhiteSpace
  cols <- decimal
  some ppmWhiteSpace
  rows <- decimal
  some ppmWhiteSpace
  string "255" --only support word8 atm
  ppmWhiteSpace
  bytes <- decimal `sepBy` some ppmWhiteSpace
  many ppmWhiteSpace
  return (fromPpmBytes rows cols bytes)

ppmWhiteSpace :: Parser ()
ppmWhiteSpace = void (Atto.takeWhile1 isSpace_w8 <|> ppmComment)

ppmComment :: Parser ByteString
ppmComment = char '#' *> Atto.takeWhile (not . isEndOfLine) <* optional endOfLine

fromPpmBytes :: Int -> Int -> [Word8] -> Array D DIM2 Color
fromPpmBytes rows cols bytes =
  let byteArr = fromListUnboxed (Z :. rows :. 3*cols) bytes

      toColor source (Z :. row :. col) = (r,g,b) where
        [r,g,b] = [source (Z :. row :. 3*col + offset) | offset <- [0,1,2]]

  in unsafeTraverse byteArr (const $ Z :. rows :. cols) toColor

toPpmBytes :: Source r Color => Array r DIM2 Color -> Array D DIM2 Word8
toPpmBytes colors =
  let (Z :. rows :. cols) = extent colors

      toByte source (Z :. row :. col) = [r,g,b] !! offset
        where (col',offset) = col `quotRem` 3
              (r,g,b) = source (Z :. row :. col')

  in unsafeTraverse colors (const $ Z :. rows :. 3*cols) toByte

toGray :: Color -> Double
toGray (r8,g8,b8) =  0.2126*r + 0.7152*g + 0.0722*b
  where [r,g,b] = fromIntegral <$> [r8,g8,b8]

fromGray :: Double -> Color
fromGray y = let x = round y in (x,x,x)

sobel :: Source r Color => Array r DIM2 Color -> Array D DIM2 Color
sobel img =
  let grey = Repa.map toGray img

      hCoeff = [stencil2| -1  0  1
                          -2  0  2
                          -1  0  1 |]

      vCoeff = [stencil2| -1 -2 -1
                           0  0  0
                           1  2  1 |]

      [hor2,vert2] = Repa.map (\x -> x*x) . forStencil2 BoundClamp grey <$> [hCoeff,vCoeff]

  in Repa.map (fromGray . sqrt) (Repa.zipWith (+) hor2 vert2)
