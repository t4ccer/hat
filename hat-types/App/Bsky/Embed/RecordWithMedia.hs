{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.RecordWithMedia where

import {-# SOURCE #-} qualified App.Bsky.Embed.External
import {-# SOURCE #-} qualified App.Bsky.Embed.Images
import {-# SOURCE #-} qualified App.Bsky.Embed.Record
import {-# SOURCE #-} qualified App.Bsky.Embed.Video
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data RecordWithMedia = RecordWithMedia
  { media :: RecordWithMediaMediaKind
  , record :: App.Bsky.Embed.Record.Record
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordWithMedia where
  parseJSON = Data.Aeson.withObject "RecordWithMedia" $ \v -> do
    media <- v Data.Aeson..: Data.Aeson.Key.fromString "media"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    pure $ RecordWithMedia media record

instance Data.Aeson.ToJSON RecordWithMedia where
  toJSON (RecordWithMedia media record) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]
  toEncoding (RecordWithMedia media record) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]

recordWithMedia'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordWithMedia -> kv
recordWithMedia'AesonFields (RecordWithMedia media record) =
  mconcat
    [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    ]

data View = View
  { media :: ViewMediaKind
  , record :: App.Bsky.Embed.Record.View
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON View where
  parseJSON = Data.Aeson.withObject "View" $ \v -> do
    media <- v Data.Aeson..: Data.Aeson.Key.fromString "media"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    pure $ View media record

instance Data.Aeson.ToJSON View where
  toJSON (View media record) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]
  toEncoding (View media record) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
view'AesonFields (View media record) =
  mconcat
    [ Data.Aeson.Key.fromString "media" Data.Aeson..= media
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    ]

data RecordWithMediaMediaKind
  = RecordWithMediaMediaKindImages App.Bsky.Embed.Images.Images
  | RecordWithMediaMediaKindVideo App.Bsky.Embed.Video.Video
  | RecordWithMediaMediaKindExternal App.Bsky.Embed.External.External
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordWithMediaMediaKind where
  parseJSON = Data.Aeson.withObject "RecordWithMediaMediaKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.images#images" -> RecordWithMediaMediaKindImages <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.video#video" -> RecordWithMediaMediaKindVideo <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.external#external" -> RecordWithMediaMediaKindExternal <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON RecordWithMediaMediaKind where
  toJSON = \case
    RecordWithMediaMediaKindImages v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#images") <> (App.Bsky.Embed.Images.images'AesonFields v))
    RecordWithMediaMediaKindVideo v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#video") <> (App.Bsky.Embed.Video.video'AesonFields v))
    RecordWithMediaMediaKindExternal v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#external") <> (App.Bsky.Embed.External.external'AesonFields v))
  toEncoding = \case
    RecordWithMediaMediaKindImages v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#images") <> (App.Bsky.Embed.Images.images'AesonFields v))
    RecordWithMediaMediaKindVideo v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#video") <> (App.Bsky.Embed.Video.video'AesonFields v))
    RecordWithMediaMediaKindExternal v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#external") <> (App.Bsky.Embed.External.external'AesonFields v))

data ViewMediaKind
  = ViewMediaKindImagesView App.Bsky.Embed.Images.View
  | ViewMediaKindVideoView App.Bsky.Embed.Video.View
  | ViewMediaKindExternalView App.Bsky.Embed.External.View
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewMediaKind where
  parseJSON = Data.Aeson.withObject "ViewMediaKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.images#view" -> ViewMediaKindImagesView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.video#view" -> ViewMediaKindVideoView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.external#view" -> ViewMediaKindExternalView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ViewMediaKind where
  toJSON = \case
    ViewMediaKindImagesView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    ViewMediaKindVideoView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    ViewMediaKindExternalView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
  toEncoding = \case
    ViewMediaKindImagesView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    ViewMediaKindVideoView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    ViewMediaKindExternalView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
