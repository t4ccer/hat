module App.Bsky.Embed.Video where

import qualified Data.Aeson

data Caption

instance Show Caption
instance Read Caption
instance Eq Caption
instance Ord Caption
instance Data.Aeson.FromJSON Caption

instance Data.Aeson.ToJSON Caption

caption'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Caption -> kv
data Video

instance Show Video
instance Read Video
instance Eq Video
instance Ord Video
instance Data.Aeson.FromJSON Video

instance Data.Aeson.ToJSON Video

video'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Video -> kv
data View

instance Show View
instance Read View
instance Eq View
instance Ord View
instance Data.Aeson.FromJSON View

instance Data.Aeson.ToJSON View

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
