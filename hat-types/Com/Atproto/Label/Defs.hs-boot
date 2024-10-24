module Com.Atproto.Label.Defs where

import qualified Data.Aeson

data Label

instance Show Label
instance Read Label
instance Eq Label
instance Ord Label
instance Data.Aeson.FromJSON Label

instance Data.Aeson.ToJSON Label

label'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Label -> kv
data LabelValue

instance Show LabelValue
instance Read LabelValue
instance Eq LabelValue
instance Ord LabelValue
instance Data.Aeson.FromJSON LabelValue

instance Data.Aeson.ToJSON LabelValue

data LabelValueDefinition

instance Show LabelValueDefinition
instance Read LabelValueDefinition
instance Eq LabelValueDefinition
instance Ord LabelValueDefinition
instance Data.Aeson.FromJSON LabelValueDefinition

instance Data.Aeson.ToJSON LabelValueDefinition

labelValueDefinition'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelValueDefinition -> kv
data LabelValueDefinitionStrings

instance Show LabelValueDefinitionStrings
instance Read LabelValueDefinitionStrings
instance Eq LabelValueDefinitionStrings
instance Ord LabelValueDefinitionStrings
instance Data.Aeson.FromJSON LabelValueDefinitionStrings

instance Data.Aeson.ToJSON LabelValueDefinitionStrings

labelValueDefinitionStrings'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelValueDefinitionStrings -> kv
data SelfLabel

instance Show SelfLabel
instance Read SelfLabel
instance Eq SelfLabel
instance Ord SelfLabel
instance Data.Aeson.FromJSON SelfLabel

instance Data.Aeson.ToJSON SelfLabel

selfLabel'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SelfLabel -> kv
data SelfLabels

instance Show SelfLabels
instance Read SelfLabels
instance Eq SelfLabels
instance Ord SelfLabels
instance Data.Aeson.FromJSON SelfLabels

instance Data.Aeson.ToJSON SelfLabels

selfLabels'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SelfLabels -> kv
