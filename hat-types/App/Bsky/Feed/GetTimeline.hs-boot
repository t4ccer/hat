module App.Bsky.Feed.GetTimeline where

import qualified Data.Aeson

data GetTimelineResult

instance Show GetTimelineResult
instance Read GetTimelineResult
instance Eq GetTimelineResult
instance Ord GetTimelineResult
instance Data.Aeson.FromJSON GetTimelineResult

instance Data.Aeson.ToJSON GetTimelineResult

getTimelineResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetTimelineResult -> kv
