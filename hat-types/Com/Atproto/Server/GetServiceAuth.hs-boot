module Com.Atproto.Server.GetServiceAuth where

import qualified Data.Aeson

data GetServiceAuthResult

instance Show GetServiceAuthResult
instance Read GetServiceAuthResult
instance Eq GetServiceAuthResult
instance Ord GetServiceAuthResult
instance Data.Aeson.FromJSON GetServiceAuthResult

instance Data.Aeson.ToJSON GetServiceAuthResult

getServiceAuthResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetServiceAuthResult -> kv
