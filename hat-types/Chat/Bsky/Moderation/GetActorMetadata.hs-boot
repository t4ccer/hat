module Chat.Bsky.Moderation.GetActorMetadata where

import qualified Data.Aeson

data GetActorMetadataResult

instance Show GetActorMetadataResult
instance Read GetActorMetadataResult
instance Eq GetActorMetadataResult
instance Ord GetActorMetadataResult
instance Data.Aeson.FromJSON GetActorMetadataResult

instance Data.Aeson.ToJSON GetActorMetadataResult

getActorMetadataResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorMetadataResult -> kv
data Metadata

instance Show Metadata
instance Read Metadata
instance Eq Metadata
instance Ord Metadata
instance Data.Aeson.FromJSON Metadata

instance Data.Aeson.ToJSON Metadata

metadata'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Metadata -> kv
