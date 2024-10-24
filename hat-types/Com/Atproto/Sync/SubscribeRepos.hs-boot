module Com.Atproto.Sync.SubscribeRepos where

import qualified Data.Aeson

data Account

instance Show Account
instance Read Account
instance Eq Account
instance Ord Account
instance Data.Aeson.FromJSON Account

instance Data.Aeson.ToJSON Account

account'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Account -> kv
data Commit

instance Show Commit
instance Read Commit
instance Eq Commit
instance Ord Commit
instance Data.Aeson.FromJSON Commit

instance Data.Aeson.ToJSON Commit

commit'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Commit -> kv
data Handle

instance Show Handle
instance Read Handle
instance Eq Handle
instance Ord Handle
instance Data.Aeson.FromJSON Handle

instance Data.Aeson.ToJSON Handle

handle'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Handle -> kv
data Identity

instance Show Identity
instance Read Identity
instance Eq Identity
instance Ord Identity
instance Data.Aeson.FromJSON Identity

instance Data.Aeson.ToJSON Identity

identity'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Identity -> kv
data Info

instance Show Info
instance Read Info
instance Eq Info
instance Ord Info
instance Data.Aeson.FromJSON Info

instance Data.Aeson.ToJSON Info

info'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Info -> kv
data Migrate

instance Show Migrate
instance Read Migrate
instance Eq Migrate
instance Ord Migrate
instance Data.Aeson.FromJSON Migrate

instance Data.Aeson.ToJSON Migrate

migrate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Migrate -> kv
data RepoOp

instance Show RepoOp
instance Read RepoOp
instance Eq RepoOp
instance Ord RepoOp
instance Data.Aeson.FromJSON RepoOp

instance Data.Aeson.ToJSON RepoOp

repoOp'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoOp -> kv
data Tombstone

instance Show Tombstone
instance Read Tombstone
instance Eq Tombstone
instance Ord Tombstone
instance Data.Aeson.FromJSON Tombstone

instance Data.Aeson.ToJSON Tombstone

tombstone'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Tombstone -> kv
