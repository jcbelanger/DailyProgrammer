import Data.List
import Text.Printf

balance xs = find isBalanced [(l, x, r) | (l, x : r) <- zip (inits xs) (tails xs)]

isBalanced (l, x, r) = wordWeight (reverse l) == wordWeight r

wordWeight = sum . zipWith (*) [1..] . map letterWeight

letterWeight = (+1) . subtract (fromEnum 'A') . fromEnum

results (Just (l, x, r)) = printf "%s %c %s - %d" l x r (wordWeight r)
results Nothing          = "DOES NOT BALANCE"

main = interact (results . balance)
