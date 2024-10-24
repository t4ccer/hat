module App.Bsky.Feed.GetPosts where

import qualified Data.Aeson

data GetPostsResult

instance Show GetPostsResult
instance Read GetPostsResult
instance Eq GetPostsResult
instance Ord GetPostsResult
instance Data.Aeson.FromJSON GetPostsResult

instance Data.Aeson.ToJSON GetPostsResult

getPostsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPostsResult -> kv
