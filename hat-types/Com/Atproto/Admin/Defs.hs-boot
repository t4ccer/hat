module Com.Atproto.Admin.Defs where

import qualified Data.Aeson

data AccountView

instance Show AccountView
instance Read AccountView
instance Eq AccountView
instance Ord AccountView
instance Data.Aeson.FromJSON AccountView

instance Data.Aeson.ToJSON AccountView

accountView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AccountView -> kv
data RepoBlobRef

instance Show RepoBlobRef
instance Read RepoBlobRef
instance Eq RepoBlobRef
instance Ord RepoBlobRef
instance Data.Aeson.FromJSON RepoBlobRef

instance Data.Aeson.ToJSON RepoBlobRef

repoBlobRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoBlobRef -> kv
data RepoRef

instance Show RepoRef
instance Read RepoRef
instance Eq RepoRef
instance Ord RepoRef
instance Data.Aeson.FromJSON RepoRef

instance Data.Aeson.ToJSON RepoRef

repoRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoRef -> kv
data StatusAttr

instance Show StatusAttr
instance Read StatusAttr
instance Eq StatusAttr
instance Ord StatusAttr
instance Data.Aeson.FromJSON StatusAttr

instance Data.Aeson.ToJSON StatusAttr

statusAttr'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StatusAttr -> kv
