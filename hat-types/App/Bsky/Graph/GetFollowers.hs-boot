module App.Bsky.Graph.GetFollowers where

import qualified Data.Aeson

data GetFollowersResult

instance Show GetFollowersResult
instance Read GetFollowersResult
instance Eq GetFollowersResult
instance Ord GetFollowersResult
instance Data.Aeson.FromJSON GetFollowersResult

instance Data.Aeson.ToJSON GetFollowersResult

getFollowersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFollowersResult -> kv
