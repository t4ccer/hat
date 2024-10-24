module App.Bsky.Labeler.Defs where

import qualified Data.Aeson

data LabelerPolicies

instance Show LabelerPolicies
instance Read LabelerPolicies
instance Eq LabelerPolicies
instance Ord LabelerPolicies
instance Data.Aeson.FromJSON LabelerPolicies

instance Data.Aeson.ToJSON LabelerPolicies

labelerPolicies'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerPolicies -> kv
data LabelerView

instance Show LabelerView
instance Read LabelerView
instance Eq LabelerView
instance Ord LabelerView
instance Data.Aeson.FromJSON LabelerView

instance Data.Aeson.ToJSON LabelerView

labelerView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerView -> kv
data LabelerViewDetailed

instance Show LabelerViewDetailed
instance Read LabelerViewDetailed
instance Eq LabelerViewDetailed
instance Ord LabelerViewDetailed
instance Data.Aeson.FromJSON LabelerViewDetailed

instance Data.Aeson.ToJSON LabelerViewDetailed

labelerViewDetailed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerViewDetailed -> kv
data LabelerViewerState

instance Show LabelerViewerState
instance Read LabelerViewerState
instance Eq LabelerViewerState
instance Ord LabelerViewerState
instance Data.Aeson.FromJSON LabelerViewerState

instance Data.Aeson.ToJSON LabelerViewerState

labelerViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerViewerState -> kv
