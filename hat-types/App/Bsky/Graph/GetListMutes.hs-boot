module App.Bsky.Graph.GetListMutes where

import qualified Data.Aeson

data GetListMutesResult

instance Show GetListMutesResult
instance Read GetListMutesResult
instance Eq GetListMutesResult
instance Ord GetListMutesResult
instance Data.Aeson.FromJSON GetListMutesResult

instance Data.Aeson.ToJSON GetListMutesResult

getListMutesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListMutesResult -> kv
