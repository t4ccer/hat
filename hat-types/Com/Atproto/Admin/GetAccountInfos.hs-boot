module Com.Atproto.Admin.GetAccountInfos where

import qualified Data.Aeson

data GetAccountInfosResult

instance Show GetAccountInfosResult
instance Read GetAccountInfosResult
instance Eq GetAccountInfosResult
instance Ord GetAccountInfosResult
instance Data.Aeson.FromJSON GetAccountInfosResult

instance Data.Aeson.ToJSON GetAccountInfosResult

getAccountInfosResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAccountInfosResult -> kv
