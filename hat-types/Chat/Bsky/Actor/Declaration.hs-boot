module Chat.Bsky.Actor.Declaration where

import qualified Data.Aeson

data Declaration

instance Show Declaration
instance Read Declaration
instance Eq Declaration
instance Ord Declaration
instance Data.Aeson.FromJSON Declaration

instance Data.Aeson.ToJSON Declaration

declaration'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Declaration -> kv
