module Com.Atproto.Repo.ApplyWrites where

import qualified Data.Aeson

data Create

instance Show Create
instance Read Create
instance Eq Create
instance Ord Create
instance Data.Aeson.FromJSON Create

instance Data.Aeson.ToJSON Create

create'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Create -> kv
data CreateResult

instance Show CreateResult
instance Read CreateResult
instance Eq CreateResult
instance Ord CreateResult
instance Data.Aeson.FromJSON CreateResult

instance Data.Aeson.ToJSON CreateResult

createResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CreateResult -> kv
data Delete

instance Show Delete
instance Read Delete
instance Eq Delete
instance Ord Delete
instance Data.Aeson.FromJSON Delete

instance Data.Aeson.ToJSON Delete

delete'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Delete -> kv
data DeleteResult

instance Show DeleteResult
instance Read DeleteResult
instance Eq DeleteResult
instance Ord DeleteResult
instance Data.Aeson.FromJSON DeleteResult

instance Data.Aeson.ToJSON DeleteResult

deleteResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DeleteResult -> kv
data Update

instance Show Update
instance Read Update
instance Eq Update
instance Ord Update
instance Data.Aeson.FromJSON Update

instance Data.Aeson.ToJSON Update

update'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Update -> kv
data UpdateResult

instance Show UpdateResult
instance Read UpdateResult
instance Eq UpdateResult
instance Ord UpdateResult
instance Data.Aeson.FromJSON UpdateResult

instance Data.Aeson.ToJSON UpdateResult

updateResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => UpdateResult -> kv
