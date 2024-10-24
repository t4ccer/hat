module App.Bsky.Embed.Images where

import qualified Data.Aeson

data Image

instance Show Image
instance Read Image
instance Eq Image
instance Ord Image
instance Data.Aeson.FromJSON Image

instance Data.Aeson.ToJSON Image

image'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Image -> kv
data Images

instance Show Images
instance Read Images
instance Eq Images
instance Ord Images
instance Data.Aeson.FromJSON Images

instance Data.Aeson.ToJSON Images

images'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Images -> kv
data View

instance Show View
instance Read View
instance Eq View
instance Ord View
instance Data.Aeson.FromJSON View

instance Data.Aeson.ToJSON View

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
data ViewImage

instance Show ViewImage
instance Read ViewImage
instance Eq ViewImage
instance Ord ViewImage
instance Data.Aeson.FromJSON ViewImage

instance Data.Aeson.ToJSON ViewImage

viewImage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewImage -> kv
