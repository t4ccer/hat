module Tools.Ozone.Team.Defs where

import qualified Data.Aeson

data Member

instance Show Member
instance Read Member
instance Eq Member
instance Ord Member
instance Data.Aeson.FromJSON Member

instance Data.Aeson.ToJSON Member

member'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Member -> kv
