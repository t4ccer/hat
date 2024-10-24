module Com.Atproto.Repo.ListMissingBlobs where

import qualified Data.Aeson

data ListMissingBlobsResult

instance Show ListMissingBlobsResult
instance Read ListMissingBlobsResult
instance Eq ListMissingBlobsResult
instance Ord ListMissingBlobsResult
instance Data.Aeson.FromJSON ListMissingBlobsResult

instance Data.Aeson.ToJSON ListMissingBlobsResult

listMissingBlobsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListMissingBlobsResult -> kv
data RecordBlob

instance Show RecordBlob
instance Read RecordBlob
instance Eq RecordBlob
instance Ord RecordBlob
instance Data.Aeson.FromJSON RecordBlob

instance Data.Aeson.ToJSON RecordBlob

recordBlob'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordBlob -> kv
