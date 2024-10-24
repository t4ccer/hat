{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Labeler.GetServices where

import {-# SOURCE #-} qualified App.Bsky.Labeler.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetServicesResult = GetServicesResult
  { views :: [GetServicesResultViewsKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetServicesResult where
  parseJSON = Data.Aeson.withObject "GetServicesResult" $ \v -> do
    views <- v Data.Aeson..: Data.Aeson.Key.fromString "views"
    pure $ GetServicesResult views

instance Data.Aeson.ToJSON GetServicesResult where
  toJSON (GetServicesResult views) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "views" Data.Aeson..= views
        ]
  toEncoding (GetServicesResult views) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "views" Data.Aeson..= views
        ]

getServicesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetServicesResult -> kv
getServicesResult'AesonFields (GetServicesResult views) =
  mconcat
    [ Data.Aeson.Key.fromString "views" Data.Aeson..= views
    ]

type GetServices = "app.bsky.labeler.getServices" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "detailed" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "dids" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetServicesResult
data GetServicesResultViewsKind
  = GetServicesResultViewsKindLabelerView App.Bsky.Labeler.Defs.LabelerView
  | GetServicesResultViewsKindLabelerViewDetailed App.Bsky.Labeler.Defs.LabelerViewDetailed
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetServicesResultViewsKind where
  parseJSON = Data.Aeson.withObject "GetServicesResultViewsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.labeler.defs#labelerView" -> GetServicesResultViewsKindLabelerView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.labeler.defs#labelerViewDetailed" -> GetServicesResultViewsKindLabelerViewDetailed <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetServicesResultViewsKind where
  toJSON = \case
    GetServicesResultViewsKindLabelerView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerView") <> (App.Bsky.Labeler.Defs.labelerView'AesonFields v))
    GetServicesResultViewsKindLabelerViewDetailed v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerViewDetailed") <> (App.Bsky.Labeler.Defs.labelerViewDetailed'AesonFields v))
  toEncoding = \case
    GetServicesResultViewsKindLabelerView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerView") <> (App.Bsky.Labeler.Defs.labelerView'AesonFields v))
    GetServicesResultViewsKindLabelerViewDetailed v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerViewDetailed") <> (App.Bsky.Labeler.Defs.labelerViewDetailed'AesonFields v))
