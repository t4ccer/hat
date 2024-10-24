module Com.Atproto.Sync.GetRepoStatus where

import qualified Data.Aeson

data GetRepoStatusResult

instance Show GetRepoStatusResult
instance Read GetRepoStatusResult
instance Eq GetRepoStatusResult
instance Ord GetRepoStatusResult
instance Data.Aeson.FromJSON GetRepoStatusResult

instance Data.Aeson.ToJSON GetRepoStatusResult

getRepoStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRepoStatusResult -> kv
