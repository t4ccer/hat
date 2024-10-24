module Chat.Bsky.Actor.Defs where

import qualified Data.Aeson

data ProfileViewBasic

instance Show ProfileViewBasic
instance Read ProfileViewBasic
instance Eq ProfileViewBasic
instance Ord ProfileViewBasic
instance Data.Aeson.FromJSON ProfileViewBasic

instance Data.Aeson.ToJSON ProfileViewBasic

profileViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewBasic -> kv
