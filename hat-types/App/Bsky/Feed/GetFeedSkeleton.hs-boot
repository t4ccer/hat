module App.Bsky.Feed.GetFeedSkeleton where

import qualified Data.Aeson

data GetFeedSkeletonResult

instance Show GetFeedSkeletonResult
instance Read GetFeedSkeletonResult
instance Eq GetFeedSkeletonResult
instance Ord GetFeedSkeletonResult
instance Data.Aeson.FromJSON GetFeedSkeletonResult

instance Data.Aeson.ToJSON GetFeedSkeletonResult

getFeedSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedSkeletonResult -> kv
