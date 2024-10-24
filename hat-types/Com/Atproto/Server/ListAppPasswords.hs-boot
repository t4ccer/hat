module Com.Atproto.Server.ListAppPasswords where

import qualified Data.Aeson

data AppPassword

instance Show AppPassword
instance Read AppPassword
instance Eq AppPassword
instance Ord AppPassword
instance Data.Aeson.FromJSON AppPassword

instance Data.Aeson.ToJSON AppPassword

appPassword'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AppPassword -> kv
data ListAppPasswordsResult

instance Show ListAppPasswordsResult
instance Read ListAppPasswordsResult
instance Eq ListAppPasswordsResult
instance Ord ListAppPasswordsResult
instance Data.Aeson.FromJSON ListAppPasswordsResult

instance Data.Aeson.ToJSON ListAppPasswordsResult

listAppPasswordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListAppPasswordsResult -> kv
