module Tools.Ozone.Moderation.GetRecords where

import qualified Data.Aeson

data GetRecordsResult

instance Show GetRecordsResult
instance Read GetRecordsResult
instance Eq GetRecordsResult
instance Ord GetRecordsResult
instance Data.Aeson.FromJSON GetRecordsResult

instance Data.Aeson.ToJSON GetRecordsResult

getRecordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecordsResult -> kv
data GetRecordsResultRecordsKind

instance Show GetRecordsResultRecordsKind
instance Read GetRecordsResultRecordsKind
instance Eq GetRecordsResultRecordsKind
instance Ord GetRecordsResultRecordsKind
instance Data.Aeson.FromJSON GetRecordsResultRecordsKind

instance Data.Aeson.ToJSON GetRecordsResultRecordsKind
