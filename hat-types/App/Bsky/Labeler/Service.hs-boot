module App.Bsky.Labeler.Service where

import qualified Data.Aeson

data Service

instance Show Service
instance Read Service
instance Eq Service
instance Ord Service
instance Data.Aeson.FromJSON Service

instance Data.Aeson.ToJSON Service

service'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Service -> kv
data ServiceLabelsKind

instance Show ServiceLabelsKind
instance Read ServiceLabelsKind
instance Eq ServiceLabelsKind
instance Ord ServiceLabelsKind
instance Data.Aeson.FromJSON ServiceLabelsKind

instance Data.Aeson.ToJSON ServiceLabelsKind
