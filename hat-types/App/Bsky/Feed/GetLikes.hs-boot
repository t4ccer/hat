module App.Bsky.Feed.GetLikes where

import qualified Data.Aeson

data Like

instance Show Like
instance Read Like
instance Eq Like
instance Ord Like
instance Data.Aeson.FromJSON Like

instance Data.Aeson.ToJSON Like

like'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Like -> kv
data GetLikesResult

instance Show GetLikesResult
instance Read GetLikesResult
instance Eq GetLikesResult
instance Ord GetLikesResult
instance Data.Aeson.FromJSON GetLikesResult

instance Data.Aeson.ToJSON GetLikesResult

getLikesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLikesResult -> kv
