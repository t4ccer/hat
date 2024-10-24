module Com.Atproto.Temp.FetchLabels where

import qualified Data.Aeson

data FetchLabelsResult

instance Show FetchLabelsResult
instance Read FetchLabelsResult
instance Eq FetchLabelsResult
instance Ord FetchLabelsResult
instance Data.Aeson.FromJSON FetchLabelsResult

instance Data.Aeson.ToJSON FetchLabelsResult

fetchLabelsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FetchLabelsResult -> kv
