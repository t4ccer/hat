module App.Bsky.Graph.GetStarterPack where

import qualified Data.Aeson

data GetStarterPackResult

instance Show GetStarterPackResult
instance Read GetStarterPackResult
instance Eq GetStarterPackResult
instance Ord GetStarterPackResult
instance Data.Aeson.FromJSON GetStarterPackResult

instance Data.Aeson.ToJSON GetStarterPackResult

getStarterPackResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetStarterPackResult -> kv
