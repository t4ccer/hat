module Com.Atproto.Repo.StrongRef where

import qualified Data.Aeson

data StrongRef

instance Show StrongRef
instance Read StrongRef
instance Eq StrongRef
instance Ord StrongRef
instance Data.Aeson.FromJSON StrongRef

instance Data.Aeson.ToJSON StrongRef

strongRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StrongRef -> kv
