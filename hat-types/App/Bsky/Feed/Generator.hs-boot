module App.Bsky.Feed.Generator where

import qualified Data.Aeson

data Generator

instance Show Generator
instance Read Generator
instance Eq Generator
instance Ord Generator
instance Data.Aeson.FromJSON Generator

instance Data.Aeson.ToJSON Generator

generator'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Generator -> kv
data GeneratorLabelsKind

instance Show GeneratorLabelsKind
instance Read GeneratorLabelsKind
instance Eq GeneratorLabelsKind
instance Ord GeneratorLabelsKind
instance Data.Aeson.FromJSON GeneratorLabelsKind

instance Data.Aeson.ToJSON GeneratorLabelsKind
