module App.Bsky.Unspecced.GetSuggestionsSkeleton where

import qualified Data.Aeson

data GetSuggestionsSkeletonResult

instance Show GetSuggestionsSkeletonResult
instance Read GetSuggestionsSkeletonResult
instance Eq GetSuggestionsSkeletonResult
instance Ord GetSuggestionsSkeletonResult
instance Data.Aeson.FromJSON GetSuggestionsSkeletonResult

instance Data.Aeson.ToJSON GetSuggestionsSkeletonResult

getSuggestionsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestionsSkeletonResult -> kv
