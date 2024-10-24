module Tools.Ozone.Server.GetConfig where

import qualified Data.Aeson

data GetConfigResult

instance Show GetConfigResult
instance Read GetConfigResult
instance Eq GetConfigResult
instance Ord GetConfigResult
instance Data.Aeson.FromJSON GetConfigResult

instance Data.Aeson.ToJSON GetConfigResult

getConfigResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConfigResult -> kv
data ServiceConfig

instance Show ServiceConfig
instance Read ServiceConfig
instance Eq ServiceConfig
instance Ord ServiceConfig
instance Data.Aeson.FromJSON ServiceConfig

instance Data.Aeson.ToJSON ServiceConfig

serviceConfig'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ServiceConfig -> kv
data ViewerConfig

instance Show ViewerConfig
instance Read ViewerConfig
instance Eq ViewerConfig
instance Ord ViewerConfig
instance Data.Aeson.FromJSON ViewerConfig

instance Data.Aeson.ToJSON ViewerConfig

viewerConfig'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerConfig -> kv
