module Com.Atproto.Repo.GetRecord where

import qualified Data.Aeson

data GetRecordResult

instance Show GetRecordResult
instance Read GetRecordResult
instance Eq GetRecordResult
instance Ord GetRecordResult
instance Data.Aeson.FromJSON GetRecordResult

instance Data.Aeson.ToJSON GetRecordResult

getRecordResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecordResult -> kv
