{-# LANGUAGE OverloadedStrings #-}

{-
https://www.reddit.com/r/dailyprogrammer/comments/3j3pvm/20150831_challenge_230_easy_json_treasure_hunt/
-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad

main = interact $ maybe "<invalid json>" search . decode . B.pack

search = T.unpack . maybe "<not found>" (T.intercalate " -> ") . go
    where go (String "dailyprogrammer") = Just []
          go (Object o) = msum [(k:) <$> go v | (k,  v) <- M.toList o]
          go (Array a ) = msum [(k:) <$> go v | (k', v) <- zip [0..] (V.toList a), let k = T.pack (show k)]
          go _          = Nothing
