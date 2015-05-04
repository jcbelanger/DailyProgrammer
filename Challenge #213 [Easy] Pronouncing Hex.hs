{-
http://www.reddit.com/r/dailyprogrammer/comments/34rxkc/20150504_challenge_213_easy_pronouncing_hex/
-}

xss <.> yss = [ xs ++ ys | xs <- xss, ys <- yss ]

dig1 = words "one two three four five six seven eight nine a bee cee dee e eff"
dig2 = dig1
       ++ words "ten eleven twelve"
       ++ words "thir four fif six seven eigh nine ab bib cleven dibble egg fleven" <.> ["teen"]
       ++ (twentyToNinety ++ ["atta"] ++ bibbityToFleventy) <.> ([] : (["-"] <.> dig1))
       where twentyToNinety = words "twen thir for fif six seven eigh nine" <.> ["ty"]
             bibbityToFleventy = words "bibbi ci dicke ebbi fleven" <.> ["ty"]
dig4 = dig2 ++ dig2 <.> [" bitey"] <.> ([] : ([" "] <.> dig2))

main = interact $ (("zero":dig4) !!) . read
