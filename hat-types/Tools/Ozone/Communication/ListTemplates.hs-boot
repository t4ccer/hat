module Tools.Ozone.Communication.ListTemplates where

import qualified Data.Aeson

data ListTemplatesResult

instance Show ListTemplatesResult
instance Read ListTemplatesResult
instance Eq ListTemplatesResult
instance Ord ListTemplatesResult
instance Data.Aeson.FromJSON ListTemplatesResult

instance Data.Aeson.ToJSON ListTemplatesResult

listTemplatesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListTemplatesResult -> kv
