module Tools.Ozone.Signature.FindCorrelation where

import qualified Data.Aeson

data FindCorrelationResult

instance Show FindCorrelationResult
instance Read FindCorrelationResult
instance Eq FindCorrelationResult
instance Ord FindCorrelationResult
instance Data.Aeson.FromJSON FindCorrelationResult

instance Data.Aeson.ToJSON FindCorrelationResult

findCorrelationResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FindCorrelationResult -> kv
