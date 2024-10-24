module App.Bsky.Feed.GetFeedGenerator where

import qualified Data.Aeson

data GetFeedGeneratorResult

instance Show GetFeedGeneratorResult
instance Read GetFeedGeneratorResult
instance Eq GetFeedGeneratorResult
instance Ord GetFeedGeneratorResult
instance Data.Aeson.FromJSON GetFeedGeneratorResult

instance Data.Aeson.ToJSON GetFeedGeneratorResult

getFeedGeneratorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedGeneratorResult -> kv
