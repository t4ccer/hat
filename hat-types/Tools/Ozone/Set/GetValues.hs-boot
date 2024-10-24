module Tools.Ozone.Set.GetValues where

import qualified Data.Aeson

data GetValuesResult

instance Show GetValuesResult
instance Read GetValuesResult
instance Eq GetValuesResult
instance Ord GetValuesResult
instance Data.Aeson.FromJSON GetValuesResult

instance Data.Aeson.ToJSON GetValuesResult

getValuesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetValuesResult -> kv
