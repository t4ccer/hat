module App.Bsky.Graph.GetStarterPacks where

import qualified Data.Aeson

data GetStarterPacksResult

instance Show GetStarterPacksResult
instance Read GetStarterPacksResult
instance Eq GetStarterPacksResult
instance Ord GetStarterPacksResult
instance Data.Aeson.FromJSON GetStarterPacksResult

instance Data.Aeson.ToJSON GetStarterPacksResult

getStarterPacksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetStarterPacksResult -> kv
