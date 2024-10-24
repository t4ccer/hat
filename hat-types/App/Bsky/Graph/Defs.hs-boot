module App.Bsky.Graph.Defs where

import qualified Data.Aeson

data ListItemView

instance Show ListItemView
instance Read ListItemView
instance Eq ListItemView
instance Ord ListItemView
instance Data.Aeson.FromJSON ListItemView

instance Data.Aeson.ToJSON ListItemView

listItemView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListItemView -> kv
data ListPurpose

instance Show ListPurpose
instance Read ListPurpose
instance Eq ListPurpose
instance Ord ListPurpose
instance Data.Aeson.FromJSON ListPurpose

instance Data.Aeson.ToJSON ListPurpose

data ListView

instance Show ListView
instance Read ListView
instance Eq ListView
instance Ord ListView
instance Data.Aeson.FromJSON ListView

instance Data.Aeson.ToJSON ListView

listView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListView -> kv
data ListViewBasic

instance Show ListViewBasic
instance Read ListViewBasic
instance Eq ListViewBasic
instance Ord ListViewBasic
instance Data.Aeson.FromJSON ListViewBasic

instance Data.Aeson.ToJSON ListViewBasic

listViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListViewBasic -> kv
data ListViewerState

instance Show ListViewerState
instance Read ListViewerState
instance Eq ListViewerState
instance Ord ListViewerState
instance Data.Aeson.FromJSON ListViewerState

instance Data.Aeson.ToJSON ListViewerState

listViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListViewerState -> kv
data NotFoundActor

instance Show NotFoundActor
instance Read NotFoundActor
instance Eq NotFoundActor
instance Ord NotFoundActor
instance Data.Aeson.FromJSON NotFoundActor

instance Data.Aeson.ToJSON NotFoundActor

notFoundActor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => NotFoundActor -> kv
data Relationship

instance Show Relationship
instance Read Relationship
instance Eq Relationship
instance Ord Relationship
instance Data.Aeson.FromJSON Relationship

instance Data.Aeson.ToJSON Relationship

relationship'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Relationship -> kv
data StarterPackView

instance Show StarterPackView
instance Read StarterPackView
instance Eq StarterPackView
instance Ord StarterPackView
instance Data.Aeson.FromJSON StarterPackView

instance Data.Aeson.ToJSON StarterPackView

starterPackView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StarterPackView -> kv
data StarterPackViewBasic

instance Show StarterPackViewBasic
instance Read StarterPackViewBasic
instance Eq StarterPackViewBasic
instance Ord StarterPackViewBasic
instance Data.Aeson.FromJSON StarterPackViewBasic

instance Data.Aeson.ToJSON StarterPackViewBasic

starterPackViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StarterPackViewBasic -> kv
