module Tools.Ozone.Signature.SearchAccounts where

import qualified Data.Aeson

data SearchAccountsResult

instance Show SearchAccountsResult
instance Read SearchAccountsResult
instance Eq SearchAccountsResult
instance Ord SearchAccountsResult
instance Data.Aeson.FromJSON SearchAccountsResult

instance Data.Aeson.ToJSON SearchAccountsResult

searchAccountsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchAccountsResult -> kv
