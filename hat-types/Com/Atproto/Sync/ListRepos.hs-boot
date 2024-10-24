module Com.Atproto.Sync.ListRepos where

import qualified Data.Aeson

data ListReposResult

instance Show ListReposResult
instance Read ListReposResult
instance Eq ListReposResult
instance Ord ListReposResult
instance Data.Aeson.FromJSON ListReposResult

instance Data.Aeson.ToJSON ListReposResult

listReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListReposResult -> kv
data Repo

instance Show Repo
instance Read Repo
instance Eq Repo
instance Ord Repo
instance Data.Aeson.FromJSON Repo

instance Data.Aeson.ToJSON Repo

repo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Repo -> kv
