module Com.Atproto.Label.QueryLabels where

import qualified Data.Aeson

data QueryLabelsResult

instance Show QueryLabelsResult
instance Read QueryLabelsResult
instance Eq QueryLabelsResult
instance Ord QueryLabelsResult
instance Data.Aeson.FromJSON QueryLabelsResult

instance Data.Aeson.ToJSON QueryLabelsResult

queryLabelsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryLabelsResult -> kv
