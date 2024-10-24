{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetRelationships where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetRelationshipsResult = GetRelationshipsResult
  { actor :: Maybe Data.Text.Text
  , relationships :: [GetRelationshipsResultRelationshipsKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRelationshipsResult where
  parseJSON = Data.Aeson.withObject "GetRelationshipsResult" $ \v -> do
    actor <- v Data.Aeson..:? Data.Aeson.Key.fromString "actor"
    relationships <- v Data.Aeson..: Data.Aeson.Key.fromString "relationships"
    pure $ GetRelationshipsResult actor relationships

instance Data.Aeson.ToJSON GetRelationshipsResult where
  toJSON (GetRelationshipsResult actor relationships) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..?= actor
        , Data.Aeson.Key.fromString "relationships" Data.Aeson..= relationships
        ]
  toEncoding (GetRelationshipsResult actor relationships) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..?= actor
        , Data.Aeson.Key.fromString "relationships" Data.Aeson..= relationships
        ]

getRelationshipsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRelationshipsResult -> kv
getRelationshipsResult'AesonFields (GetRelationshipsResult actor relationships) =
  mconcat
    [ Data.Aeson.Key.fromString "actor" Data.Aeson..?= actor
    , Data.Aeson.Key.fromString "relationships" Data.Aeson..= relationships
    ]

type GetRelationships = "app.bsky.graph.getRelationships" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "others" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRelationshipsResult
data GetRelationshipsResultRelationshipsKind
  = GetRelationshipsResultRelationshipsKindRelationship App.Bsky.Graph.Defs.Relationship
  | GetRelationshipsResultRelationshipsKindNotFoundActor App.Bsky.Graph.Defs.NotFoundActor
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRelationshipsResultRelationshipsKind where
  parseJSON = Data.Aeson.withObject "GetRelationshipsResultRelationshipsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.graph.defs#relationship" -> GetRelationshipsResultRelationshipsKindRelationship <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.graph.defs#notFoundActor" -> GetRelationshipsResultRelationshipsKindNotFoundActor <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetRelationshipsResultRelationshipsKind where
  toJSON = \case
    GetRelationshipsResultRelationshipsKindRelationship v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#relationship") <> (App.Bsky.Graph.Defs.relationship'AesonFields v))
    GetRelationshipsResultRelationshipsKindNotFoundActor v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#notFoundActor") <> (App.Bsky.Graph.Defs.notFoundActor'AesonFields v))
  toEncoding = \case
    GetRelationshipsResultRelationshipsKindRelationship v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#relationship") <> (App.Bsky.Graph.Defs.relationship'AesonFields v))
    GetRelationshipsResultRelationshipsKindNotFoundActor v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#notFoundActor") <> (App.Bsky.Graph.Defs.notFoundActor'AesonFields v))
