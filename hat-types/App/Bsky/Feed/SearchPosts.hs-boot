module App.Bsky.Feed.SearchPosts where

import qualified Data.Aeson

data SearchPostsResult

instance Show SearchPostsResult
instance Read SearchPostsResult
instance Eq SearchPostsResult
instance Ord SearchPostsResult
instance Data.Aeson.FromJSON SearchPostsResult

instance Data.Aeson.ToJSON SearchPostsResult

searchPostsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchPostsResult -> kv
