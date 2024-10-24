module Com.Atproto.Identity.ResolveHandle where

import qualified Data.Aeson

data ResolveHandleResult

instance Show ResolveHandleResult
instance Read ResolveHandleResult
instance Eq ResolveHandleResult
instance Ord ResolveHandleResult
instance Data.Aeson.FromJSON ResolveHandleResult

instance Data.Aeson.ToJSON ResolveHandleResult

resolveHandleResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ResolveHandleResult -> kv
