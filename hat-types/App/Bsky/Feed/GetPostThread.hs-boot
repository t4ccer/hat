module App.Bsky.Feed.GetPostThread where

import qualified Data.Aeson

data GetPostThreadResult

instance Show GetPostThreadResult
instance Read GetPostThreadResult
instance Eq GetPostThreadResult
instance Ord GetPostThreadResult
instance Data.Aeson.FromJSON GetPostThreadResult

instance Data.Aeson.ToJSON GetPostThreadResult

getPostThreadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPostThreadResult -> kv
data GetPostThreadResultThreadKind

instance Show GetPostThreadResultThreadKind
instance Read GetPostThreadResultThreadKind
instance Eq GetPostThreadResultThreadKind
instance Ord GetPostThreadResultThreadKind
instance Data.Aeson.FromJSON GetPostThreadResultThreadKind

instance Data.Aeson.ToJSON GetPostThreadResultThreadKind
