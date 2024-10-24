module Tools.Ozone.Moderation.GetRepos where

import qualified Data.Aeson

data GetReposResult

instance Show GetReposResult
instance Read GetReposResult
instance Eq GetReposResult
instance Ord GetReposResult
instance Data.Aeson.FromJSON GetReposResult

instance Data.Aeson.ToJSON GetReposResult

getReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetReposResult -> kv
data GetReposResultReposKind

instance Show GetReposResultReposKind
instance Read GetReposResultReposKind
instance Eq GetReposResultReposKind
instance Ord GetReposResultReposKind
instance Data.Aeson.FromJSON GetReposResultReposKind

instance Data.Aeson.ToJSON GetReposResultReposKind
