module App.Bsky.Unspecced.GetTaggedSuggestions where

import qualified Data.Aeson

data GetTaggedSuggestionsResult

instance Show GetTaggedSuggestionsResult
instance Read GetTaggedSuggestionsResult
instance Eq GetTaggedSuggestionsResult
instance Ord GetTaggedSuggestionsResult
instance Data.Aeson.FromJSON GetTaggedSuggestionsResult

instance Data.Aeson.ToJSON GetTaggedSuggestionsResult

getTaggedSuggestionsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetTaggedSuggestionsResult -> kv
data Suggestion

instance Show Suggestion
instance Read Suggestion
instance Eq Suggestion
instance Ord Suggestion
instance Data.Aeson.FromJSON Suggestion

instance Data.Aeson.ToJSON Suggestion

suggestion'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Suggestion -> kv
