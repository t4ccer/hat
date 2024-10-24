module Com.Atproto.Admin.GetInviteCodes where

import qualified Data.Aeson

data GetInviteCodesResult

instance Show GetInviteCodesResult
instance Read GetInviteCodesResult
instance Eq GetInviteCodesResult
instance Ord GetInviteCodesResult
instance Data.Aeson.FromJSON GetInviteCodesResult

instance Data.Aeson.ToJSON GetInviteCodesResult

getInviteCodesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetInviteCodesResult -> kv
