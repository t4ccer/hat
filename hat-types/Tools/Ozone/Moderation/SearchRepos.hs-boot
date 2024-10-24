module Tools.Ozone.Moderation.SearchRepos where

import qualified Data.Aeson

data SearchReposResult

instance Show SearchReposResult
instance Read SearchReposResult
instance Eq SearchReposResult
instance Ord SearchReposResult
instance Data.Aeson.FromJSON SearchReposResult

instance Data.Aeson.ToJSON SearchReposResult

searchReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchReposResult -> kv
