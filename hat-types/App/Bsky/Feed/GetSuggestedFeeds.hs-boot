module App.Bsky.Feed.GetSuggestedFeeds where

import qualified Data.Aeson

data GetSuggestedFeedsResult

instance Show GetSuggestedFeedsResult
instance Read GetSuggestedFeedsResult
instance Eq GetSuggestedFeedsResult
instance Ord GetSuggestedFeedsResult
instance Data.Aeson.FromJSON GetSuggestedFeedsResult

instance Data.Aeson.ToJSON GetSuggestedFeedsResult

getSuggestedFeedsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestedFeedsResult -> kv
