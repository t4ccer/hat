module App.Bsky.Video.Defs where

import qualified Data.Aeson

data JobStatus

instance Show JobStatus
instance Read JobStatus
instance Eq JobStatus
instance Ord JobStatus
instance Data.Aeson.FromJSON JobStatus

instance Data.Aeson.ToJSON JobStatus

jobStatus'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => JobStatus -> kv
