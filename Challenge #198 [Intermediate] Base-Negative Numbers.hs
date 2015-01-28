import Data.Char

toBase10 base ('-':num) = -(toBase10 base num)
toBase10 base num = sum $ zipWith (*) powers (reverse digits)
    where powers = map (base^) [0..]
          digits = map digitToInt num

quotRem' a b = let (a', b') = quotRem a b
               in  if b' < 0
                   then (a' + 1, b' + (abs b))
                   else (a', b')

fromBase10 base num = sign ++ map intToDigit digits
    where step (q, r) = quotRem' q base
          steps = takeWhile (not . isDone) $ iterate step (abs num, 0)
          isDone (q, r) = q==0 && r==0
          digits = reverse . map snd . drop 1 $ steps
          sign = if num < 0 then "-" else ""

main = interact $ \input -> let [n, num] = words input
                                base = read n
                                num10 = toBase10 base num
                            in  fromBase10 (-base) num10