{-
https://www.reddit.com/r/dailyprogrammer/comments/3kx6oh/20150914_challenge_232_easy_palindromes/?sort=top
-}

import Data.Char
import Data.Bool

main = interact $ bool "Not a palindrome" "Palindrome" . (reverse >>= (==)) . map toLower . filter isAlphaNum . dropWhile (/= '\n')
