module App.Bsky.Feed.GetFeed where

import qualified Data.Aeson

data GetFeedResult

instance Show GetFeedResult
instance Read GetFeedResult
instance Eq GetFeedResult
instance Ord GetFeedResult
instance Data.Aeson.FromJSON GetFeedResult

instance Data.Aeson.ToJSON GetFeedResult

getFeedResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedResult -> kv
