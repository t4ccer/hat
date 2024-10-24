module App.Bsky.Graph.GetFollows where

import qualified Data.Aeson

data GetFollowsResult

instance Show GetFollowsResult
instance Read GetFollowsResult
instance Eq GetFollowsResult
instance Ord GetFollowsResult
instance Data.Aeson.FromJSON GetFollowsResult

instance Data.Aeson.ToJSON GetFollowsResult

getFollowsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFollowsResult -> kv
