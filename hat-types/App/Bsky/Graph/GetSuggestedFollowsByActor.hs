{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetSuggestedFollowsByActor where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetSuggestedFollowsByActorResult = GetSuggestedFollowsByActorResult
  { isFallback :: Maybe Bool
  , suggestions :: [App.Bsky.Actor.Defs.ProfileView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSuggestedFollowsByActorResult where
  parseJSON = Data.Aeson.withObject "GetSuggestedFollowsByActorResult" $ \v -> do
    isFallback <- v Data.Aeson..:? Data.Aeson.Key.fromString "isFallback"
    suggestions <- v Data.Aeson..: Data.Aeson.Key.fromString "suggestions"
    pure $ GetSuggestedFollowsByActorResult isFallback suggestions

instance Data.Aeson.ToJSON GetSuggestedFollowsByActorResult where
  toJSON (GetSuggestedFollowsByActorResult isFallback suggestions) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "isFallback" Data.Aeson..?= isFallback
        , Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
        ]
  toEncoding (GetSuggestedFollowsByActorResult isFallback suggestions) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "isFallback" Data.Aeson..?= isFallback
        , Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
        ]

getSuggestedFollowsByActorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestedFollowsByActorResult -> kv
getSuggestedFollowsByActorResult'AesonFields (GetSuggestedFollowsByActorResult isFallback suggestions) =
  mconcat
    [ Data.Aeson.Key.fromString "isFallback" Data.Aeson..?= isFallback
    , Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
    ]

type GetSuggestedFollowsByActor = "app.bsky.graph.getSuggestedFollowsByActor" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetSuggestedFollowsByActorResult
