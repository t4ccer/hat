module App.Bsky.Actor.SearchActors where

import qualified Data.Aeson

data SearchActorsResult

instance Show SearchActorsResult
instance Read SearchActorsResult
instance Eq SearchActorsResult
instance Ord SearchActorsResult
instance Data.Aeson.FromJSON SearchActorsResult

instance Data.Aeson.ToJSON SearchActorsResult

searchActorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsResult -> kv
