module Com.Atproto.Repo.Defs where

import qualified Data.Aeson

data CommitMeta

instance Show CommitMeta
instance Read CommitMeta
instance Eq CommitMeta
instance Ord CommitMeta
instance Data.Aeson.FromJSON CommitMeta

instance Data.Aeson.ToJSON CommitMeta

commitMeta'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CommitMeta -> kv
