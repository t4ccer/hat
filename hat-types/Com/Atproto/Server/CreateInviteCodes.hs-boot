module Com.Atproto.Server.CreateInviteCodes where

import qualified Data.Aeson

data AccountCodes

instance Show AccountCodes
instance Read AccountCodes
instance Eq AccountCodes
instance Ord AccountCodes
instance Data.Aeson.FromJSON AccountCodes

instance Data.Aeson.ToJSON AccountCodes

accountCodes'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AccountCodes -> kv
