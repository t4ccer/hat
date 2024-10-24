module App.Bsky.Unspecced.GetPopularFeedGenerators where

import qualified Data.Aeson

data GetPopularFeedGeneratorsResult

instance Show GetPopularFeedGeneratorsResult
instance Read GetPopularFeedGeneratorsResult
instance Eq GetPopularFeedGeneratorsResult
instance Ord GetPopularFeedGeneratorsResult
instance Data.Aeson.FromJSON GetPopularFeedGeneratorsResult

instance Data.Aeson.ToJSON GetPopularFeedGeneratorsResult

getPopularFeedGeneratorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPopularFeedGeneratorsResult -> kv
