module App.Bsky.Graph.GetKnownFollowers where

import qualified Data.Aeson

data GetKnownFollowersResult

instance Show GetKnownFollowersResult
instance Read GetKnownFollowersResult
instance Eq GetKnownFollowersResult
instance Ord GetKnownFollowersResult
instance Data.Aeson.FromJSON GetKnownFollowersResult

instance Data.Aeson.ToJSON GetKnownFollowersResult

getKnownFollowersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetKnownFollowersResult -> kv
