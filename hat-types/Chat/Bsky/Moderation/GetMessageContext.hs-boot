module Chat.Bsky.Moderation.GetMessageContext where

import qualified Data.Aeson

data GetMessageContextResult

instance Show GetMessageContextResult
instance Read GetMessageContextResult
instance Eq GetMessageContextResult
instance Ord GetMessageContextResult
instance Data.Aeson.FromJSON GetMessageContextResult

instance Data.Aeson.ToJSON GetMessageContextResult

getMessageContextResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMessageContextResult -> kv
data GetMessageContextResultMessagesKind

instance Show GetMessageContextResultMessagesKind
instance Read GetMessageContextResultMessagesKind
instance Eq GetMessageContextResultMessagesKind
instance Ord GetMessageContextResultMessagesKind
instance Data.Aeson.FromJSON GetMessageContextResultMessagesKind

instance Data.Aeson.ToJSON GetMessageContextResultMessagesKind
