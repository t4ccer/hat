module App.Bsky.Graph.GetMutes where

import qualified Data.Aeson

data GetMutesResult

instance Show GetMutesResult
instance Read GetMutesResult
instance Eq GetMutesResult
instance Ord GetMutesResult
instance Data.Aeson.FromJSON GetMutesResult

instance Data.Aeson.ToJSON GetMutesResult

getMutesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMutesResult -> kv
