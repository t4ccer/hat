module App.Bsky.Video.GetUploadLimits where

import qualified Data.Aeson

data GetUploadLimitsResult

instance Show GetUploadLimitsResult
instance Read GetUploadLimitsResult
instance Eq GetUploadLimitsResult
instance Ord GetUploadLimitsResult
instance Data.Aeson.FromJSON GetUploadLimitsResult

instance Data.Aeson.ToJSON GetUploadLimitsResult

getUploadLimitsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetUploadLimitsResult -> kv
