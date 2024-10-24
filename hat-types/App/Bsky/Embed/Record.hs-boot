module App.Bsky.Embed.Record where

import qualified Data.Aeson

data Record

instance Show Record
instance Read Record
instance Eq Record
instance Ord Record
instance Data.Aeson.FromJSON Record

instance Data.Aeson.ToJSON Record

record'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Record -> kv
data View

instance Show View
instance Read View
instance Eq View
instance Ord View
instance Data.Aeson.FromJSON View

instance Data.Aeson.ToJSON View

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
data ViewBlocked

instance Show ViewBlocked
instance Read ViewBlocked
instance Eq ViewBlocked
instance Ord ViewBlocked
instance Data.Aeson.FromJSON ViewBlocked

instance Data.Aeson.ToJSON ViewBlocked

viewBlocked'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewBlocked -> kv
data ViewDetached

instance Show ViewDetached
instance Read ViewDetached
instance Eq ViewDetached
instance Ord ViewDetached
instance Data.Aeson.FromJSON ViewDetached

instance Data.Aeson.ToJSON ViewDetached

viewDetached'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewDetached -> kv
data ViewNotFound

instance Show ViewNotFound
instance Read ViewNotFound
instance Eq ViewNotFound
instance Ord ViewNotFound
instance Data.Aeson.FromJSON ViewNotFound

instance Data.Aeson.ToJSON ViewNotFound

viewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewNotFound -> kv
data ViewRecord

instance Show ViewRecord
instance Read ViewRecord
instance Eq ViewRecord
instance Ord ViewRecord
instance Data.Aeson.FromJSON ViewRecord

instance Data.Aeson.ToJSON ViewRecord

viewRecord'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewRecord -> kv
data ViewRecordEmbedsKind

instance Show ViewRecordEmbedsKind
instance Read ViewRecordEmbedsKind
instance Eq ViewRecordEmbedsKind
instance Ord ViewRecordEmbedsKind
instance Data.Aeson.FromJSON ViewRecordEmbedsKind

instance Data.Aeson.ToJSON ViewRecordEmbedsKind

data ViewRecordKind

instance Show ViewRecordKind
instance Read ViewRecordKind
instance Eq ViewRecordKind
instance Ord ViewRecordKind
instance Data.Aeson.FromJSON ViewRecordKind

instance Data.Aeson.ToJSON ViewRecordKind
