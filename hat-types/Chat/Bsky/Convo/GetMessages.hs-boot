module Chat.Bsky.Convo.GetMessages where

import qualified Data.Aeson

data GetMessagesResult

instance Show GetMessagesResult
instance Read GetMessagesResult
instance Eq GetMessagesResult
instance Ord GetMessagesResult
instance Data.Aeson.FromJSON GetMessagesResult

instance Data.Aeson.ToJSON GetMessagesResult

getMessagesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMessagesResult -> kv
data GetMessagesResultMessagesKind

instance Show GetMessagesResultMessagesKind
instance Read GetMessagesResultMessagesKind
instance Eq GetMessagesResultMessagesKind
instance Ord GetMessagesResultMessagesKind
instance Data.Aeson.FromJSON GetMessagesResultMessagesKind

instance Data.Aeson.ToJSON GetMessagesResultMessagesKind
