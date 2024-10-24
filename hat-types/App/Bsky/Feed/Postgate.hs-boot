module App.Bsky.Feed.Postgate where

import qualified Data.Aeson

data DisableRule

instance Show DisableRule
instance Read DisableRule
instance Eq DisableRule
instance Ord DisableRule
instance Data.Aeson.FromJSON DisableRule

instance Data.Aeson.ToJSON DisableRule

disableRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DisableRule -> kv
data Postgate

instance Show Postgate
instance Read Postgate
instance Eq Postgate
instance Ord Postgate
instance Data.Aeson.FromJSON Postgate

instance Data.Aeson.ToJSON Postgate

postgate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Postgate -> kv
data PostgateEmbeddingRulesKind

instance Show PostgateEmbeddingRulesKind
instance Read PostgateEmbeddingRulesKind
instance Eq PostgateEmbeddingRulesKind
instance Ord PostgateEmbeddingRulesKind
instance Data.Aeson.FromJSON PostgateEmbeddingRulesKind

instance Data.Aeson.ToJSON PostgateEmbeddingRulesKind
