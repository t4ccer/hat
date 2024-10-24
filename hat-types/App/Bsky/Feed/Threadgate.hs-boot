module App.Bsky.Feed.Threadgate where

import qualified Data.Aeson

data FollowingRule

instance Show FollowingRule
instance Read FollowingRule
instance Eq FollowingRule
instance Ord FollowingRule
instance Data.Aeson.FromJSON FollowingRule

instance Data.Aeson.ToJSON FollowingRule

followingRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FollowingRule -> kv
data ListRule

instance Show ListRule
instance Read ListRule
instance Eq ListRule
instance Ord ListRule
instance Data.Aeson.FromJSON ListRule

instance Data.Aeson.ToJSON ListRule

listRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListRule -> kv
data Threadgate

instance Show Threadgate
instance Read Threadgate
instance Eq Threadgate
instance Ord Threadgate
instance Data.Aeson.FromJSON Threadgate

instance Data.Aeson.ToJSON Threadgate

threadgate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Threadgate -> kv
data MentionRule

instance Show MentionRule
instance Read MentionRule
instance Eq MentionRule
instance Ord MentionRule
instance Data.Aeson.FromJSON MentionRule

instance Data.Aeson.ToJSON MentionRule

mentionRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MentionRule -> kv
data ThreadgateAllowKind

instance Show ThreadgateAllowKind
instance Read ThreadgateAllowKind
instance Eq ThreadgateAllowKind
instance Ord ThreadgateAllowKind
instance Data.Aeson.FromJSON ThreadgateAllowKind

instance Data.Aeson.ToJSON ThreadgateAllowKind
