module App.Bsky.Embed.Defs where

import qualified Data.Aeson

data AspectRatio

instance Show AspectRatio
instance Read AspectRatio
instance Eq AspectRatio
instance Ord AspectRatio
instance Data.Aeson.FromJSON AspectRatio

instance Data.Aeson.ToJSON AspectRatio

aspectRatio'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AspectRatio -> kv
