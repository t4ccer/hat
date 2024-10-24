module App.Bsky.Notification.ListNotifications where

import qualified Data.Aeson

data ListNotificationsResult

instance Show ListNotificationsResult
instance Read ListNotificationsResult
instance Eq ListNotificationsResult
instance Ord ListNotificationsResult
instance Data.Aeson.FromJSON ListNotificationsResult

instance Data.Aeson.ToJSON ListNotificationsResult

listNotificationsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListNotificationsResult -> kv
data Notification

instance Show Notification
instance Read Notification
instance Eq Notification
instance Ord Notification
instance Data.Aeson.FromJSON Notification

instance Data.Aeson.ToJSON Notification

notification'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Notification -> kv
