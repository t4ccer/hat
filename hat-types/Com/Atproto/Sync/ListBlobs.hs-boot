module Com.Atproto.Sync.ListBlobs where

import qualified Data.Aeson

data ListBlobsResult

instance Show ListBlobsResult
instance Read ListBlobsResult
instance Eq ListBlobsResult
instance Ord ListBlobsResult
instance Data.Aeson.FromJSON ListBlobsResult

instance Data.Aeson.ToJSON ListBlobsResult

listBlobsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListBlobsResult -> kv
