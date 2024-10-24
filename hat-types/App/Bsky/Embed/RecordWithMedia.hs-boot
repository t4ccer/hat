module App.Bsky.Embed.RecordWithMedia where

import qualified Data.Aeson

data RecordWithMedia

instance Show RecordWithMedia
instance Read RecordWithMedia
instance Eq RecordWithMedia
instance Ord RecordWithMedia
instance Data.Aeson.FromJSON RecordWithMedia

instance Data.Aeson.ToJSON RecordWithMedia

recordWithMedia'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordWithMedia -> kv
data View

instance Show View
instance Read View
instance Eq View
instance Ord View
instance Data.Aeson.FromJSON View

instance Data.Aeson.ToJSON View

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
data RecordWithMediaMediaKind

instance Show RecordWithMediaMediaKind
instance Read RecordWithMediaMediaKind
instance Eq RecordWithMediaMediaKind
instance Ord RecordWithMediaMediaKind
instance Data.Aeson.FromJSON RecordWithMediaMediaKind

instance Data.Aeson.ToJSON RecordWithMediaMediaKind

data ViewMediaKind

instance Show ViewMediaKind
instance Read ViewMediaKind
instance Eq ViewMediaKind
instance Ord ViewMediaKind
instance Data.Aeson.FromJSON ViewMediaKind

instance Data.Aeson.ToJSON ViewMediaKind
