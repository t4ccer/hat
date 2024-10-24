module App.Bsky.Graph.GetLists where

import qualified Data.Aeson

data GetListsResult

instance Show GetListsResult
instance Read GetListsResult
instance Eq GetListsResult
instance Ord GetListsResult
instance Data.Aeson.FromJSON GetListsResult

instance Data.Aeson.ToJSON GetListsResult

getListsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListsResult -> kv
