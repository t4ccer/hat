module Tools.Ozone.Set.Defs where

import qualified Data.Aeson

data Set

instance Show Set
instance Read Set
instance Eq Set
instance Ord Set
instance Data.Aeson.FromJSON Set

instance Data.Aeson.ToJSON Set

set'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Set -> kv
data SetView

instance Show SetView
instance Read SetView
instance Eq SetView
instance Ord SetView
instance Data.Aeson.FromJSON SetView

instance Data.Aeson.ToJSON SetView

setView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SetView -> kv
