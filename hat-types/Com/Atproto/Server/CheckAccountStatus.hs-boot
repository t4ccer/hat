module Com.Atproto.Server.CheckAccountStatus where

import qualified Data.Aeson

data CheckAccountStatusResult

instance Show CheckAccountStatusResult
instance Read CheckAccountStatusResult
instance Eq CheckAccountStatusResult
instance Ord CheckAccountStatusResult
instance Data.Aeson.FromJSON CheckAccountStatusResult

instance Data.Aeson.ToJSON CheckAccountStatusResult

checkAccountStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CheckAccountStatusResult -> kv
