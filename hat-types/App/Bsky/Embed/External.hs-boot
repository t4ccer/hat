module App.Bsky.Embed.External where

import qualified Data.Aeson

data External

instance Show External
instance Read External
instance Eq External
instance Ord External
instance Data.Aeson.FromJSON External

instance Data.Aeson.ToJSON External

external'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => External -> kv
data ExternalMain

instance Show ExternalMain
instance Read ExternalMain
instance Eq ExternalMain
instance Ord ExternalMain
instance Data.Aeson.FromJSON ExternalMain

instance Data.Aeson.ToJSON ExternalMain

externalMain'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ExternalMain -> kv
data View

instance Show View
instance Read View
instance Eq View
instance Ord View
instance Data.Aeson.FromJSON View

instance Data.Aeson.ToJSON View

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
data ViewExternal

instance Show ViewExternal
instance Read ViewExternal
instance Eq ViewExternal
instance Ord ViewExternal
instance Data.Aeson.FromJSON ViewExternal

instance Data.Aeson.ToJSON ViewExternal

viewExternal'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewExternal -> kv
