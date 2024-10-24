module App.Bsky.Actor.GetProfiles where

import qualified Data.Aeson

data GetProfilesResult

instance Show GetProfilesResult
instance Read GetProfilesResult
instance Eq GetProfilesResult
instance Ord GetProfilesResult
instance Data.Aeson.FromJSON GetProfilesResult

instance Data.Aeson.ToJSON GetProfilesResult

getProfilesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetProfilesResult -> kv
