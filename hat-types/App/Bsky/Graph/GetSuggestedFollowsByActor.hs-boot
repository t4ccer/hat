module App.Bsky.Graph.GetSuggestedFollowsByActor where

import qualified Data.Aeson

data GetSuggestedFollowsByActorResult

instance Show GetSuggestedFollowsByActorResult
instance Read GetSuggestedFollowsByActorResult
instance Eq GetSuggestedFollowsByActorResult
instance Ord GetSuggestedFollowsByActorResult
instance Data.Aeson.FromJSON GetSuggestedFollowsByActorResult

instance Data.Aeson.ToJSON GetSuggestedFollowsByActorResult

getSuggestedFollowsByActorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestedFollowsByActorResult -> kv
