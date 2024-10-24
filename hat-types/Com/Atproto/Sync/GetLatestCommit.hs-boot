module Com.Atproto.Sync.GetLatestCommit where

import qualified Data.Aeson

data GetLatestCommitResult

instance Show GetLatestCommitResult
instance Read GetLatestCommitResult
instance Eq GetLatestCommitResult
instance Ord GetLatestCommitResult
instance Data.Aeson.FromJSON GetLatestCommitResult

instance Data.Aeson.ToJSON GetLatestCommitResult

getLatestCommitResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLatestCommitResult -> kv
