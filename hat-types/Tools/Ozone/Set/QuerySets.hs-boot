module Tools.Ozone.Set.QuerySets where

import qualified Data.Aeson

data QuerySetsResult

instance Show QuerySetsResult
instance Read QuerySetsResult
instance Eq QuerySetsResult
instance Ord QuerySetsResult
instance Data.Aeson.FromJSON QuerySetsResult

instance Data.Aeson.ToJSON QuerySetsResult

querySetsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QuerySetsResult -> kv
