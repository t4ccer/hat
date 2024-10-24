module Com.Atproto.Server.GetAccountInviteCodes where

import qualified Data.Aeson

data GetAccountInviteCodesResult

instance Show GetAccountInviteCodesResult
instance Read GetAccountInviteCodesResult
instance Eq GetAccountInviteCodesResult
instance Ord GetAccountInviteCodesResult
instance Data.Aeson.FromJSON GetAccountInviteCodesResult

instance Data.Aeson.ToJSON GetAccountInviteCodesResult

getAccountInviteCodesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAccountInviteCodesResult -> kv
