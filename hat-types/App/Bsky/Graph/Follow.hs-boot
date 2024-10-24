module App.Bsky.Graph.Follow where

import qualified Data.Aeson

data Follow

instance Show Follow
instance Read Follow
instance Eq Follow
instance Ord Follow
instance Data.Aeson.FromJSON Follow

instance Data.Aeson.ToJSON Follow

follow'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Follow -> kv
