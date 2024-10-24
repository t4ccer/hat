module App.Bsky.Video.GetJobStatus where

import qualified Data.Aeson

data GetJobStatusResult

instance Show GetJobStatusResult
instance Read GetJobStatusResult
instance Eq GetJobStatusResult
instance Ord GetJobStatusResult
instance Data.Aeson.FromJSON GetJobStatusResult

instance Data.Aeson.ToJSON GetJobStatusResult

getJobStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetJobStatusResult -> kv
