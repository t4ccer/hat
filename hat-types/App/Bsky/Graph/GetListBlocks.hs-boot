module App.Bsky.Graph.GetListBlocks where

import qualified Data.Aeson

data GetListBlocksResult

instance Show GetListBlocksResult
instance Read GetListBlocksResult
instance Eq GetListBlocksResult
instance Ord GetListBlocksResult
instance Data.Aeson.FromJSON GetListBlocksResult

instance Data.Aeson.ToJSON GetListBlocksResult

getListBlocksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListBlocksResult -> kv
