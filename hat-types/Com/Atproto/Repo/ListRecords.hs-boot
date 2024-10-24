module Com.Atproto.Repo.ListRecords where

import qualified Data.Aeson

data ListRecordsResult

instance Show ListRecordsResult
instance Read ListRecordsResult
instance Eq ListRecordsResult
instance Ord ListRecordsResult
instance Data.Aeson.FromJSON ListRecordsResult

instance Data.Aeson.ToJSON ListRecordsResult

listRecordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListRecordsResult -> kv
data Record

instance Show Record
instance Read Record
instance Eq Record
instance Ord Record
instance Data.Aeson.FromJSON Record

instance Data.Aeson.ToJSON Record

record'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Record -> kv
