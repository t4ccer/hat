module App.Bsky.Actor.SearchActorsTypeahead where

import qualified Data.Aeson

data SearchActorsTypeaheadResult

instance Show SearchActorsTypeaheadResult
instance Read SearchActorsTypeaheadResult
instance Eq SearchActorsTypeaheadResult
instance Ord SearchActorsTypeaheadResult
instance Data.Aeson.FromJSON SearchActorsTypeaheadResult

instance Data.Aeson.ToJSON SearchActorsTypeaheadResult

searchActorsTypeaheadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsTypeaheadResult -> kv
