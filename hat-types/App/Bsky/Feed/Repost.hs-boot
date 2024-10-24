module App.Bsky.Feed.Repost where

import qualified Data.Aeson

data Repost

instance Show Repost
instance Read Repost
instance Eq Repost
instance Ord Repost
instance Data.Aeson.FromJSON Repost

instance Data.Aeson.ToJSON Repost

repost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Repost -> kv
