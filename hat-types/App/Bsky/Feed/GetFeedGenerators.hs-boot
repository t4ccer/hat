module App.Bsky.Feed.GetFeedGenerators where

import qualified Data.Aeson

data GetFeedGeneratorsResult

instance Show GetFeedGeneratorsResult
instance Read GetFeedGeneratorsResult
instance Eq GetFeedGeneratorsResult
instance Ord GetFeedGeneratorsResult
instance Data.Aeson.FromJSON GetFeedGeneratorsResult

instance Data.Aeson.ToJSON GetFeedGeneratorsResult

getFeedGeneratorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedGeneratorsResult -> kv
