module App.Bsky.Unspecced.SearchPostsSkeleton where

import qualified Data.Aeson

data SearchPostsSkeletonResult

instance Show SearchPostsSkeletonResult
instance Read SearchPostsSkeletonResult
instance Eq SearchPostsSkeletonResult
instance Ord SearchPostsSkeletonResult
instance Data.Aeson.FromJSON SearchPostsSkeletonResult

instance Data.Aeson.ToJSON SearchPostsSkeletonResult

searchPostsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchPostsSkeletonResult -> kv
