{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.Record where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified App.Bsky.Embed.External
import {-# SOURCE #-} qualified App.Bsky.Embed.Images
import {-# SOURCE #-} qualified App.Bsky.Embed.RecordWithMedia
import {-# SOURCE #-} qualified App.Bsky.Embed.Video
import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import {-# SOURCE #-} qualified App.Bsky.Labeler.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Record = Record
  { record :: Com.Atproto.Repo.StrongRef.StrongRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Record where
  parseJSON = Data.Aeson.withObject "Record" $ \v -> do
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    pure $ Record record

instance Data.Aeson.ToJSON Record where
  toJSON (Record record) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]
  toEncoding (Record record) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]

record'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Record -> kv
record'AesonFields (Record record) =
  mconcat
    [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
    ]

data View = View
  { record :: ViewRecordKind
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON View where
  parseJSON = Data.Aeson.withObject "View" $ \v -> do
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    pure $ View record

instance Data.Aeson.ToJSON View where
  toJSON (View record) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]
  toEncoding (View record) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
        ]

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
view'AesonFields (View record) =
  mconcat
    [ Data.Aeson.Key.fromString "record" Data.Aeson..= record
    ]

data ViewBlocked = ViewBlocked
  { author :: App.Bsky.Feed.Defs.BlockedAuthor
  , blocked :: Bool
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewBlocked where
  parseJSON = Data.Aeson.withObject "ViewBlocked" $ \v -> do
    author <- v Data.Aeson..: Data.Aeson.Key.fromString "author"
    blocked <- v Data.Aeson..: Data.Aeson.Key.fromString "blocked"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ ViewBlocked author blocked uri

instance Data.Aeson.ToJSON ViewBlocked where
  toJSON (ViewBlocked author blocked uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (ViewBlocked author blocked uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

viewBlocked'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewBlocked -> kv
viewBlocked'AesonFields (ViewBlocked author blocked uri) =
  mconcat
    [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
    , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data ViewDetached = ViewDetached
  { detached :: Bool
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewDetached where
  parseJSON = Data.Aeson.withObject "ViewDetached" $ \v -> do
    detached <- v Data.Aeson..: Data.Aeson.Key.fromString "detached"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ ViewDetached detached uri

instance Data.Aeson.ToJSON ViewDetached where
  toJSON (ViewDetached detached uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "detached" Data.Aeson..= detached
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (ViewDetached detached uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "detached" Data.Aeson..= detached
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

viewDetached'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewDetached -> kv
viewDetached'AesonFields (ViewDetached detached uri) =
  mconcat
    [ Data.Aeson.Key.fromString "detached" Data.Aeson..= detached
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data ViewNotFound = ViewNotFound
  { notFound :: Bool
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewNotFound where
  parseJSON = Data.Aeson.withObject "ViewNotFound" $ \v -> do
    notFound <- v Data.Aeson..: Data.Aeson.Key.fromString "notFound"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ ViewNotFound notFound uri

instance Data.Aeson.ToJSON ViewNotFound where
  toJSON (ViewNotFound notFound uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (ViewNotFound notFound uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

viewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewNotFound -> kv
viewNotFound'AesonFields (ViewNotFound notFound uri) =
  mconcat
    [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data ViewRecord = ViewRecord
  { author :: App.Bsky.Actor.Defs.ProfileViewBasic
  , cid :: Data.Text.Text
  , embeds :: Maybe [ViewRecordEmbedsKind]
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , likeCount :: Maybe Integer
  , quoteCount :: Maybe Integer
  , replyCount :: Maybe Integer
  , repostCount :: Maybe Integer
  , uri :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewRecord where
  parseJSON = Data.Aeson.withObject "ViewRecord" $ \v -> do
    author <- v Data.Aeson..: Data.Aeson.Key.fromString "author"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    embeds <- v Data.Aeson..:? Data.Aeson.Key.fromString "embeds"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    likeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "likeCount"
    quoteCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "quoteCount"
    replyCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "replyCount"
    repostCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "repostCount"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ ViewRecord author cid embeds indexedAt labels likeCount quoteCount replyCount repostCount uri value

instance Data.Aeson.ToJSON ViewRecord where
  toJSON (ViewRecord author cid embeds indexedAt labels likeCount quoteCount replyCount repostCount uri value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "embeds" Data.Aeson..?= embeds
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
        , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
        , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (ViewRecord author cid embeds indexedAt labels likeCount quoteCount replyCount repostCount uri value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "embeds" Data.Aeson..?= embeds
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
        , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
        , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

viewRecord'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewRecord -> kv
viewRecord'AesonFields (ViewRecord author cid embeds indexedAt labels likeCount quoteCount replyCount repostCount uri value) =
  mconcat
    [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "embeds" Data.Aeson..?= embeds
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
    , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
    , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
    , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data ViewRecordEmbedsKind
  = ViewRecordEmbedsKindImagesView App.Bsky.Embed.Images.View
  | ViewRecordEmbedsKindVideoView App.Bsky.Embed.Video.View
  | ViewRecordEmbedsKindExternalView App.Bsky.Embed.External.View
  | ViewRecordEmbedsKindRecordView View
  | ViewRecordEmbedsKindRecordWithMediaView App.Bsky.Embed.RecordWithMedia.View
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewRecordEmbedsKind where
  parseJSON = Data.Aeson.withObject "ViewRecordEmbedsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.images#view" -> ViewRecordEmbedsKindImagesView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.video#view" -> ViewRecordEmbedsKindVideoView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.external#view" -> ViewRecordEmbedsKindExternalView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#view" -> ViewRecordEmbedsKindRecordView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.recordWithMedia#view" -> ViewRecordEmbedsKindRecordWithMediaView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ViewRecordEmbedsKind where
  toJSON = \case
    ViewRecordEmbedsKindImagesView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    ViewRecordEmbedsKindVideoView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    ViewRecordEmbedsKindExternalView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
    ViewRecordEmbedsKindRecordView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
    ViewRecordEmbedsKindRecordWithMediaView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#view") <> (App.Bsky.Embed.RecordWithMedia.view'AesonFields v))
  toEncoding = \case
    ViewRecordEmbedsKindImagesView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    ViewRecordEmbedsKindVideoView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    ViewRecordEmbedsKindExternalView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
    ViewRecordEmbedsKindRecordView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
    ViewRecordEmbedsKindRecordWithMediaView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#view") <> (App.Bsky.Embed.RecordWithMedia.view'AesonFields v))

data ViewRecordKind
  = ViewRecordKindViewRecord ViewRecord
  | ViewRecordKindViewNotFound ViewNotFound
  | ViewRecordKindViewBlocked ViewBlocked
  | ViewRecordKindViewDetached ViewDetached
  | ViewRecordKindGeneratorView App.Bsky.Feed.Defs.GeneratorView
  | ViewRecordKindListView App.Bsky.Graph.Defs.ListView
  | ViewRecordKindLabelerView App.Bsky.Labeler.Defs.LabelerView
  | ViewRecordKindStarterPackViewBasic App.Bsky.Graph.Defs.StarterPackViewBasic
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewRecordKind where
  parseJSON = Data.Aeson.withObject "ViewRecordKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.record#viewRecord" -> ViewRecordKindViewRecord <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#viewNotFound" -> ViewRecordKindViewNotFound <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#viewBlocked" -> ViewRecordKindViewBlocked <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#viewDetached" -> ViewRecordKindViewDetached <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#generatorView" -> ViewRecordKindGeneratorView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.graph.defs#listView" -> ViewRecordKindListView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.labeler.defs#labelerView" -> ViewRecordKindLabelerView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.graph.defs#starterPackViewBasic" -> ViewRecordKindStarterPackViewBasic <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ViewRecordKind where
  toJSON = \case
    ViewRecordKindViewRecord v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewRecord") <> (App.Bsky.Embed.Record.viewRecord'AesonFields v))
    ViewRecordKindViewNotFound v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewNotFound") <> (App.Bsky.Embed.Record.viewNotFound'AesonFields v))
    ViewRecordKindViewBlocked v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewBlocked") <> (App.Bsky.Embed.Record.viewBlocked'AesonFields v))
    ViewRecordKindViewDetached v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewDetached") <> (App.Bsky.Embed.Record.viewDetached'AesonFields v))
    ViewRecordKindGeneratorView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#generatorView") <> (App.Bsky.Feed.Defs.generatorView'AesonFields v))
    ViewRecordKindListView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#listView") <> (App.Bsky.Graph.Defs.listView'AesonFields v))
    ViewRecordKindLabelerView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerView") <> (App.Bsky.Labeler.Defs.labelerView'AesonFields v))
    ViewRecordKindStarterPackViewBasic v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#starterPackViewBasic") <> (App.Bsky.Graph.Defs.starterPackViewBasic'AesonFields v))
  toEncoding = \case
    ViewRecordKindViewRecord v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewRecord") <> (App.Bsky.Embed.Record.viewRecord'AesonFields v))
    ViewRecordKindViewNotFound v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewNotFound") <> (App.Bsky.Embed.Record.viewNotFound'AesonFields v))
    ViewRecordKindViewBlocked v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewBlocked") <> (App.Bsky.Embed.Record.viewBlocked'AesonFields v))
    ViewRecordKindViewDetached v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#viewDetached") <> (App.Bsky.Embed.Record.viewDetached'AesonFields v))
    ViewRecordKindGeneratorView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#generatorView") <> (App.Bsky.Feed.Defs.generatorView'AesonFields v))
    ViewRecordKindListView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#listView") <> (App.Bsky.Graph.Defs.listView'AesonFields v))
    ViewRecordKindLabelerView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.labeler.defs#labelerView") <> (App.Bsky.Labeler.Defs.labelerView'AesonFields v))
    ViewRecordKindStarterPackViewBasic v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.graph.defs#starterPackViewBasic") <> (App.Bsky.Graph.Defs.starterPackViewBasic'AesonFields v))
