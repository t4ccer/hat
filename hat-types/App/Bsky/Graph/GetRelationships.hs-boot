module App.Bsky.Graph.GetRelationships where

import qualified Data.Aeson

data GetRelationshipsResult

instance Show GetRelationshipsResult
instance Read GetRelationshipsResult
instance Eq GetRelationshipsResult
instance Ord GetRelationshipsResult
instance Data.Aeson.FromJSON GetRelationshipsResult

instance Data.Aeson.ToJSON GetRelationshipsResult

getRelationshipsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRelationshipsResult -> kv
data GetRelationshipsResultRelationshipsKind

instance Show GetRelationshipsResultRelationshipsKind
instance Read GetRelationshipsResultRelationshipsKind
instance Eq GetRelationshipsResultRelationshipsKind
instance Ord GetRelationshipsResultRelationshipsKind
instance Data.Aeson.FromJSON GetRelationshipsResultRelationshipsKind

instance Data.Aeson.ToJSON GetRelationshipsResultRelationshipsKind
