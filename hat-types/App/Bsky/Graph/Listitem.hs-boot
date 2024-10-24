module App.Bsky.Graph.Listitem where

import qualified Data.Aeson

data Listitem

instance Show Listitem
instance Read Listitem
instance Eq Listitem
instance Ord Listitem
instance Data.Aeson.FromJSON Listitem

instance Data.Aeson.ToJSON Listitem

listitem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Listitem -> kv
