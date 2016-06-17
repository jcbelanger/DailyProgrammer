{-
https://www.reddit.com/r/dailyprogrammer/comments/4o74p3/20160615_challenge_271_intermediate_making_waves/
-}

import           Control.Applicative
import           Data.Attoparsec.Text hiding (D)
import           Data.Foldable
import           Data.Text.IO         as TIO
import           Euterpea
import           Control.Arrow

main :: IO ()
main = either error play . parseOnly challenge =<< TIO.getContents

challenge :: Parser (Music Pitch)
challenge = do
    sampleRate <- decimal
    endOfLine
    let toSeconds milis = fromInteger milis / 1000
    duration <- toSeconds <$> decimal
    endOfLine
    line <$> some (musicP duration)

musicP :: Dur -> Parser (Music Pitch)
musicP dur = restP dur <|> noteP dur 4

restP :: Dur -> Parser (Music Pitch)
restP dur = rest dur <$ char '_'

noteP :: Dur -> Octave -> Parser (Music Pitch)
noteP dur oct = asum (zipWith parser [A,B,C,D,E,F,G] "ABCDEFG")
    where parser pch ch = note dur (pch, oct) <$ char ch
