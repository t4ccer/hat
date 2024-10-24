module Tools.Ozone.Signature.Defs where

import qualified Data.Aeson

data SigDetail

instance Show SigDetail
instance Read SigDetail
instance Eq SigDetail
instance Ord SigDetail
instance Data.Aeson.FromJSON SigDetail

instance Data.Aeson.ToJSON SigDetail

sigDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SigDetail -> kv
