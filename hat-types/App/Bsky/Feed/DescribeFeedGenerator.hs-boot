module App.Bsky.Feed.DescribeFeedGenerator where

import qualified Data.Aeson

data Feed

instance Show Feed
instance Read Feed
instance Eq Feed
instance Ord Feed
instance Data.Aeson.FromJSON Feed

instance Data.Aeson.ToJSON Feed

feed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Feed -> kv
data Links

instance Show Links
instance Read Links
instance Eq Links
instance Ord Links
instance Data.Aeson.FromJSON Links

instance Data.Aeson.ToJSON Links

links'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Links -> kv
data DescribeFeedGeneratorResult

instance Show DescribeFeedGeneratorResult
instance Read DescribeFeedGeneratorResult
instance Eq DescribeFeedGeneratorResult
instance Ord DescribeFeedGeneratorResult
instance Data.Aeson.FromJSON DescribeFeedGeneratorResult

instance Data.Aeson.ToJSON DescribeFeedGeneratorResult

describeFeedGeneratorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeFeedGeneratorResult -> kv
