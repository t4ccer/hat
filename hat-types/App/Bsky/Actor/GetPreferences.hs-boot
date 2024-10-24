module App.Bsky.Actor.GetPreferences where

import qualified Data.Aeson

data GetPreferencesResult

instance Show GetPreferencesResult
instance Read GetPreferencesResult
instance Eq GetPreferencesResult
instance Ord GetPreferencesResult
instance Data.Aeson.FromJSON GetPreferencesResult

instance Data.Aeson.ToJSON GetPreferencesResult

getPreferencesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPreferencesResult -> kv
