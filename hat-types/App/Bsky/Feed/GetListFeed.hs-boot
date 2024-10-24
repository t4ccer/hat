module App.Bsky.Feed.GetListFeed where

import qualified Data.Aeson

data GetListFeedResult

instance Show GetListFeedResult
instance Read GetListFeedResult
instance Eq GetListFeedResult
instance Ord GetListFeedResult
instance Data.Aeson.FromJSON GetListFeedResult

instance Data.Aeson.ToJSON GetListFeedResult

getListFeedResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListFeedResult -> kv
