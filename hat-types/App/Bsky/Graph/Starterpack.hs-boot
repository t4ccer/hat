module App.Bsky.Graph.Starterpack where

import qualified Data.Aeson

data FeedItem

instance Show FeedItem
instance Read FeedItem
instance Eq FeedItem
instance Ord FeedItem
instance Data.Aeson.FromJSON FeedItem

instance Data.Aeson.ToJSON FeedItem

feedItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedItem -> kv
data Starterpack

instance Show Starterpack
instance Read Starterpack
instance Eq Starterpack
instance Ord Starterpack
instance Data.Aeson.FromJSON Starterpack

instance Data.Aeson.ToJSON Starterpack

starterpack'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Starterpack -> kv
