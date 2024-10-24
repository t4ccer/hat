module App.Bsky.Graph.GetBlocks where

import qualified Data.Aeson

data GetBlocksResult

instance Show GetBlocksResult
instance Read GetBlocksResult
instance Eq GetBlocksResult
instance Ord GetBlocksResult
instance Data.Aeson.FromJSON GetBlocksResult

instance Data.Aeson.ToJSON GetBlocksResult

getBlocksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetBlocksResult -> kv
