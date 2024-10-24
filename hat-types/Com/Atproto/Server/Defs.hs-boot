module Com.Atproto.Server.Defs where

import qualified Data.Aeson

data InviteCode

instance Show InviteCode
instance Read InviteCode
instance Eq InviteCode
instance Ord InviteCode
instance Data.Aeson.FromJSON InviteCode

instance Data.Aeson.ToJSON InviteCode

inviteCode'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InviteCode -> kv
data InviteCodeUse

instance Show InviteCodeUse
instance Read InviteCodeUse
instance Eq InviteCodeUse
instance Ord InviteCodeUse
instance Data.Aeson.FromJSON InviteCodeUse

instance Data.Aeson.ToJSON InviteCodeUse

inviteCodeUse'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InviteCodeUse -> kv
