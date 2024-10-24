module App.Bsky.Richtext.Facet where

import qualified Data.Aeson

data ByteSlice

instance Show ByteSlice
instance Read ByteSlice
instance Eq ByteSlice
instance Ord ByteSlice
instance Data.Aeson.FromJSON ByteSlice

instance Data.Aeson.ToJSON ByteSlice

byteSlice'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ByteSlice -> kv
data Link

instance Show Link
instance Read Link
instance Eq Link
instance Ord Link
instance Data.Aeson.FromJSON Link

instance Data.Aeson.ToJSON Link

link'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Link -> kv
data Facet

instance Show Facet
instance Read Facet
instance Eq Facet
instance Ord Facet
instance Data.Aeson.FromJSON Facet

instance Data.Aeson.ToJSON Facet

facet'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Facet -> kv
data Mention

instance Show Mention
instance Read Mention
instance Eq Mention
instance Ord Mention
instance Data.Aeson.FromJSON Mention

instance Data.Aeson.ToJSON Mention

mention'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Mention -> kv
data Tag

instance Show Tag
instance Read Tag
instance Eq Tag
instance Ord Tag
instance Data.Aeson.FromJSON Tag

instance Data.Aeson.ToJSON Tag

tag'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Tag -> kv
data FacetFeaturesKind

instance Show FacetFeaturesKind
instance Read FacetFeaturesKind
instance Eq FacetFeaturesKind
instance Ord FacetFeaturesKind
instance Data.Aeson.FromJSON FacetFeaturesKind

instance Data.Aeson.ToJSON FacetFeaturesKind
