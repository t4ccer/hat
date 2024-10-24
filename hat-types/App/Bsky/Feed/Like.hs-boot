module App.Bsky.Feed.Like where

import qualified Data.Aeson

data Like

instance Show Like
instance Read Like
instance Eq Like
instance Ord Like
instance Data.Aeson.FromJSON Like

instance Data.Aeson.ToJSON Like

like'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Like -> kv
