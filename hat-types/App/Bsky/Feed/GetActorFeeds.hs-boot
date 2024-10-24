module App.Bsky.Feed.GetActorFeeds where

import qualified Data.Aeson

data GetActorFeedsResult

instance Show GetActorFeedsResult
instance Read GetActorFeedsResult
instance Eq GetActorFeedsResult
instance Ord GetActorFeedsResult
instance Data.Aeson.FromJSON GetActorFeedsResult

instance Data.Aeson.ToJSON GetActorFeedsResult

getActorFeedsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorFeedsResult -> kv
