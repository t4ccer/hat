module App.Bsky.Notification.GetUnreadCount where

import qualified Data.Aeson

data GetUnreadCountResult

instance Show GetUnreadCountResult
instance Read GetUnreadCountResult
instance Eq GetUnreadCountResult
instance Ord GetUnreadCountResult
instance Data.Aeson.FromJSON GetUnreadCountResult

instance Data.Aeson.ToJSON GetUnreadCountResult

getUnreadCountResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetUnreadCountResult -> kv
