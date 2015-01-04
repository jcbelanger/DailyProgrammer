{-|
* [2014-12-28] Challenge #195 [Easy] Symbolic Link Resolution

http://www.reddit.com/r/dailyprogrammer/comments/2qmz12/20141228_challenge_195_easy_symbolic_link/

WORK IN PROGRESS!
-}

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Function
import Data.List

links = parseLink <$> ["/bin/thing:/bin/thing-3"
                      ,"/bin/thing-3:/bin/thing-3.2"
                      ,"/bin/thing-3.2/include:/usr/include"
                      ,"/usr/include/SDL:/usr/local/include/SDL"]

target = "/bin/thing/hello/world"

output = "/usr/local/include/SDL/stan"
          


--t = getFirst . foldMap (First . (`Map.lookup` linkMap)) . tries $ target

resolvePath target links = case lookupFirst (pathInits target) links of
    Nothing -> target
    Just from -> resolvePath next links
        where remain = stripPrefix match target
              next = from ++ remain

parseLink = second tail . break (== ':')

pathInits = map concat . inits . groupBy ((&&) `on` (/= '/'))

lookupFirst keys map = concatMap (\key -> ((== key) . fst) links) keys

keys = pathInits target
f = zipWith lookup keys (repeat links)

main = do
    --n <- read <$> getLine
    --links <- parseLink <$> replicateM n getLine
    --target <- getLine
    print $ resolvePath target links