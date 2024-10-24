module App.Bsky.Graph.Block where

import qualified Data.Aeson

data Block

instance Show Block
instance Read Block
instance Eq Block
instance Ord Block
instance Data.Aeson.FromJSON Block

instance Data.Aeson.ToJSON Block

block'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Block -> kv
