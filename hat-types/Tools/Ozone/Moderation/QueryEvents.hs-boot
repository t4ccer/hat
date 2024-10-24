module Tools.Ozone.Moderation.QueryEvents where

import qualified Data.Aeson

data QueryEventsResult

instance Show QueryEventsResult
instance Read QueryEventsResult
instance Eq QueryEventsResult
instance Ord QueryEventsResult
instance Data.Aeson.FromJSON QueryEventsResult

instance Data.Aeson.ToJSON QueryEventsResult

queryEventsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryEventsResult -> kv
