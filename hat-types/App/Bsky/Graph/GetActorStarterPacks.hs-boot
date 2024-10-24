module App.Bsky.Graph.GetActorStarterPacks where

import qualified Data.Aeson

data GetActorStarterPacksResult

instance Show GetActorStarterPacksResult
instance Read GetActorStarterPacksResult
instance Eq GetActorStarterPacksResult
instance Ord GetActorStarterPacksResult
instance Data.Aeson.FromJSON GetActorStarterPacksResult

instance Data.Aeson.ToJSON GetActorStarterPacksResult

getActorStarterPacksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorStarterPacksResult -> kv
