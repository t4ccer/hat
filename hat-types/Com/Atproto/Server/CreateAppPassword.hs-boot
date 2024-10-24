module Com.Atproto.Server.CreateAppPassword where

import qualified Data.Aeson

data AppPassword

instance Show AppPassword
instance Read AppPassword
instance Eq AppPassword
instance Ord AppPassword
instance Data.Aeson.FromJSON AppPassword

instance Data.Aeson.ToJSON AppPassword

appPassword'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AppPassword -> kv
