module App.Bsky.Graph.Listblock where

import qualified Data.Aeson

data Listblock

instance Show Listblock
instance Read Listblock
instance Eq Listblock
instance Ord Listblock
instance Data.Aeson.FromJSON Listblock

instance Data.Aeson.ToJSON Listblock

listblock'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Listblock -> kv
