{-
https://www.reddit.com/r/dailyprogrammer/comments/4v5h3u/20160729_challenge_277_hard_trippy_julia_fractals/
-}

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Complex
import Data.List

main :: IO ()
main = do
    [width,height] <- map read . words <$> getContents
    let f z = z^2 - (0.221 :+ 0.713)
        thres = 2
        img = juliaImg f thres width height
    writePng "out.png" img


juliaImg :: (Complex Double -> Complex Double) -> Double -> Int -> Int -> Image Pixel8
juliaImg f thres width height = generateImage (curry toIntesity) width height
  where
    toIntesity :: (Int,Int) -> Pixel8
    toIntesity = genericLength . take 255 . takeWhile stable . iterate f . toImaginary

    toImaginary :: (Int,Int) -> Complex Double
    toImaginary (x,y) = let a = fromIntegral (width  `div` 2 - x) / fromIntegral width
                            b = fromIntegral (height `div` 2 - y) / fromIntegral height
                      in a :+ b

    stable :: Complex Double -> Bool
    stable z = magnitude z <= thres
