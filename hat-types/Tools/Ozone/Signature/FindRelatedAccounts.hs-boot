module Tools.Ozone.Signature.FindRelatedAccounts where

import qualified Data.Aeson

data FindRelatedAccountsResult

instance Show FindRelatedAccountsResult
instance Read FindRelatedAccountsResult
instance Eq FindRelatedAccountsResult
instance Ord FindRelatedAccountsResult
instance Data.Aeson.FromJSON FindRelatedAccountsResult

instance Data.Aeson.ToJSON FindRelatedAccountsResult

findRelatedAccountsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FindRelatedAccountsResult -> kv
data RelatedAccount

instance Show RelatedAccount
instance Read RelatedAccount
instance Eq RelatedAccount
instance Ord RelatedAccount
instance Data.Aeson.FromJSON RelatedAccount

instance Data.Aeson.ToJSON RelatedAccount

relatedAccount'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RelatedAccount -> kv
