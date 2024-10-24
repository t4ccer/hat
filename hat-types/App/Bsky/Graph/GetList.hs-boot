module App.Bsky.Graph.GetList where

import qualified Data.Aeson

data GetListResult

instance Show GetListResult
instance Read GetListResult
instance Eq GetListResult
instance Ord GetListResult
instance Data.Aeson.FromJSON GetListResult

instance Data.Aeson.ToJSON GetListResult

getListResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListResult -> kv
