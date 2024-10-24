module App.Bsky.Labeler.GetServices where

import qualified Data.Aeson

data GetServicesResult

instance Show GetServicesResult
instance Read GetServicesResult
instance Eq GetServicesResult
instance Ord GetServicesResult
instance Data.Aeson.FromJSON GetServicesResult

instance Data.Aeson.ToJSON GetServicesResult

getServicesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetServicesResult -> kv
data GetServicesResultViewsKind

instance Show GetServicesResultViewsKind
instance Read GetServicesResultViewsKind
instance Eq GetServicesResultViewsKind
instance Ord GetServicesResultViewsKind
instance Data.Aeson.FromJSON GetServicesResultViewsKind

instance Data.Aeson.ToJSON GetServicesResultViewsKind
