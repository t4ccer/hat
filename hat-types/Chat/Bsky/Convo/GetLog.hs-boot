module Chat.Bsky.Convo.GetLog where

import qualified Data.Aeson

data GetLogResult

instance Show GetLogResult
instance Read GetLogResult
instance Eq GetLogResult
instance Ord GetLogResult
instance Data.Aeson.FromJSON GetLogResult

instance Data.Aeson.ToJSON GetLogResult

getLogResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLogResult -> kv
data GetLogResultLogsKind

instance Show GetLogResultLogsKind
instance Read GetLogResultLogsKind
instance Eq GetLogResultLogsKind
instance Ord GetLogResultLogsKind
instance Data.Aeson.FromJSON GetLogResultLogsKind

instance Data.Aeson.ToJSON GetLogResultLogsKind
