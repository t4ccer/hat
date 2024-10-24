module App.Bsky.Feed.GetAuthorFeed where

import qualified Data.Aeson

data GetAuthorFeedResult

instance Show GetAuthorFeedResult
instance Read GetAuthorFeedResult
instance Eq GetAuthorFeedResult
instance Ord GetAuthorFeedResult
instance Data.Aeson.FromJSON GetAuthorFeedResult

instance Data.Aeson.ToJSON GetAuthorFeedResult

getAuthorFeedResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAuthorFeedResult -> kv
