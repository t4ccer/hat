module App.Bsky.Unspecced.Defs where

import qualified Data.Aeson

data SkeletonSearchActor

instance Show SkeletonSearchActor
instance Read SkeletonSearchActor
instance Eq SkeletonSearchActor
instance Ord SkeletonSearchActor
instance Data.Aeson.FromJSON SkeletonSearchActor

instance Data.Aeson.ToJSON SkeletonSearchActor

skeletonSearchActor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonSearchActor -> kv
data SkeletonSearchPost

instance Show SkeletonSearchPost
instance Read SkeletonSearchPost
instance Eq SkeletonSearchPost
instance Ord SkeletonSearchPost
instance Data.Aeson.FromJSON SkeletonSearchPost

instance Data.Aeson.ToJSON SkeletonSearchPost

skeletonSearchPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonSearchPost -> kv
