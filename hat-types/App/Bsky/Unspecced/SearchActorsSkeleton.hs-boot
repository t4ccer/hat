module App.Bsky.Unspecced.SearchActorsSkeleton where

import qualified Data.Aeson

data SearchActorsSkeletonResult

instance Show SearchActorsSkeletonResult
instance Read SearchActorsSkeletonResult
instance Eq SearchActorsSkeletonResult
instance Ord SearchActorsSkeletonResult
instance Data.Aeson.FromJSON SearchActorsSkeletonResult

instance Data.Aeson.ToJSON SearchActorsSkeletonResult

searchActorsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsSkeletonResult -> kv
