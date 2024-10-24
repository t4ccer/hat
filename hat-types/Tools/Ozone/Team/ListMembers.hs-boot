module Tools.Ozone.Team.ListMembers where

import qualified Data.Aeson

data ListMembersResult

instance Show ListMembersResult
instance Read ListMembersResult
instance Eq ListMembersResult
instance Ord ListMembersResult
instance Data.Aeson.FromJSON ListMembersResult

instance Data.Aeson.ToJSON ListMembersResult

listMembersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListMembersResult -> kv
