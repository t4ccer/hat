module Chat.Bsky.Convo.GetConvoForMembers where

import qualified Data.Aeson

data GetConvoForMembersResult

instance Show GetConvoForMembersResult
instance Read GetConvoForMembersResult
instance Eq GetConvoForMembersResult
instance Ord GetConvoForMembersResult
instance Data.Aeson.FromJSON GetConvoForMembersResult

instance Data.Aeson.ToJSON GetConvoForMembersResult

getConvoForMembersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConvoForMembersResult -> kv
