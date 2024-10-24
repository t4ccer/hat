module App.Bsky.Feed.GetRepostedBy where

import qualified Data.Aeson

data GetRepostedByResult

instance Show GetRepostedByResult
instance Read GetRepostedByResult
instance Eq GetRepostedByResult
instance Ord GetRepostedByResult
instance Data.Aeson.FromJSON GetRepostedByResult

instance Data.Aeson.ToJSON GetRepostedByResult

getRepostedByResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRepostedByResult -> kv
