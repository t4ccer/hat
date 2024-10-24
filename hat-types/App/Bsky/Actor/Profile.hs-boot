module App.Bsky.Actor.Profile where

import qualified Data.Aeson

data Profile

instance Show Profile
instance Read Profile
instance Eq Profile
instance Ord Profile
instance Data.Aeson.FromJSON Profile

instance Data.Aeson.ToJSON Profile

profile'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Profile -> kv
data ProfileLabelsKind

instance Show ProfileLabelsKind
instance Read ProfileLabelsKind
instance Eq ProfileLabelsKind
instance Ord ProfileLabelsKind
instance Data.Aeson.FromJSON ProfileLabelsKind

instance Data.Aeson.ToJSON ProfileLabelsKind
