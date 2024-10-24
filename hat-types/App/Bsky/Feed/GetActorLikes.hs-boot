module App.Bsky.Feed.GetActorLikes where

import qualified Data.Aeson

data GetActorLikesResult

instance Show GetActorLikesResult
instance Read GetActorLikesResult
instance Eq GetActorLikesResult
instance Ord GetActorLikesResult
instance Data.Aeson.FromJSON GetActorLikesResult

instance Data.Aeson.ToJSON GetActorLikesResult

getActorLikesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorLikesResult -> kv
