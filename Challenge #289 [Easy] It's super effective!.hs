{-
https://www.reddit.com/r/dailyprogrammer/comments/5961a5/20161024_challenge_289_easy_its_super_effective/
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text       as Text
import           Network.Wreq

damageFrom :: Text -> Maybe Double
damageFrom "half_damage_from"   = Just (1/2)
damageFrom "no_damage_from"     = Just 0
damageFrom "double_damage_from" = Just 2
damageFrom _                    = Nothing

challenge :: String -> String -> IO (Text,[Text])
challenge move pokemon = do
  moveResp <- get ("http://pokeapi.co/api/v2/move/" <> move)
  pokemonResp <- get ("http://pokeapi.co/api/v2/pokemon/" <> pokemon)
  let moveType = moveResp ^. responseBody . key "type" . key "name" . _String
  let pokemonTypes = pokemonResp ^.. responseBody . key "types" . values . key "type" . key "name" . _String
  pokemonTypeResp <- get ("http://pokeapi.co/api/v2/type/" <> Text.unpack (pokemonTypes !! 0))
  let asdfa = pokemonTypeResp ^.. responseBody . key "damage_relations" . members . values . key "name" . _String
  return (moveType, asdfa)
