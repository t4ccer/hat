module Com.Atproto.Label.SubscribeLabels where

import qualified Data.Aeson

data Info

instance Show Info
instance Read Info
instance Eq Info
instance Ord Info
instance Data.Aeson.FromJSON Info

instance Data.Aeson.ToJSON Info

info'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Info -> kv
data Labels

instance Show Labels
instance Read Labels
instance Eq Labels
instance Ord Labels
instance Data.Aeson.FromJSON Labels

instance Data.Aeson.ToJSON Labels

labels'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Labels -> kv
