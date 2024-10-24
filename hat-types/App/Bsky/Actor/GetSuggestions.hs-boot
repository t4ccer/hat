module App.Bsky.Actor.GetSuggestions where

import qualified Data.Aeson

data GetSuggestionsResult

instance Show GetSuggestionsResult
instance Read GetSuggestionsResult
instance Eq GetSuggestionsResult
instance Ord GetSuggestionsResult
instance Data.Aeson.FromJSON GetSuggestionsResult

instance Data.Aeson.ToJSON GetSuggestionsResult

getSuggestionsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestionsResult -> kv
