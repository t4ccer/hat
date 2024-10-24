module Tools.Ozone.Communication.Defs where

import qualified Data.Aeson

data TemplateView

instance Show TemplateView
instance Read TemplateView
instance Eq TemplateView
instance Ord TemplateView
instance Data.Aeson.FromJSON TemplateView

instance Data.Aeson.ToJSON TemplateView

templateView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => TemplateView -> kv
