module App.Bsky.Graph.List where

import qualified Data.Aeson

data List

instance Show List
instance Read List
instance Eq List
instance Ord List
instance Data.Aeson.FromJSON List

instance Data.Aeson.ToJSON List

list'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => List -> kv
data ListLabelsKind

instance Show ListLabelsKind
instance Read ListLabelsKind
instance Eq ListLabelsKind
instance Ord ListLabelsKind
instance Data.Aeson.FromJSON ListLabelsKind

instance Data.Aeson.ToJSON ListLabelsKind
