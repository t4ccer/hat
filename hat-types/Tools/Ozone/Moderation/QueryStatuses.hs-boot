module Tools.Ozone.Moderation.QueryStatuses where

import qualified Data.Aeson

data QueryStatusesResult

instance Show QueryStatusesResult
instance Read QueryStatusesResult
instance Eq QueryStatusesResult
instance Ord QueryStatusesResult
instance Data.Aeson.FromJSON QueryStatusesResult

instance Data.Aeson.ToJSON QueryStatusesResult

queryStatusesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryStatusesResult -> kv
