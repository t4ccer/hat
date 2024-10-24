{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Post where

import {-# SOURCE #-} qualified App.Bsky.Embed.External
import {-# SOURCE #-} qualified App.Bsky.Embed.Images
import {-# SOURCE #-} qualified App.Bsky.Embed.Record
import {-# SOURCE #-} qualified App.Bsky.Embed.RecordWithMedia
import {-# SOURCE #-} qualified App.Bsky.Embed.Video
import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Entity = Entity
  { index :: TextSlice
  , type' :: Data.Text.Text
  , value :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Entity where
  parseJSON = Data.Aeson.withObject "Entity" $ \v -> do
    index <- v Data.Aeson..: Data.Aeson.Key.fromString "index"
    type' <- v Data.Aeson..: Data.Aeson.Key.fromString "type"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ Entity index type' value

instance Data.Aeson.ToJSON Entity where
  toJSON (Entity index type' value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "index" Data.Aeson..= index
        , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (Entity index type' value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "index" Data.Aeson..= index
        , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

entity'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Entity -> kv
entity'AesonFields (Entity index type' value) =
  mconcat
    [ Data.Aeson.Key.fromString "index" Data.Aeson..= index
    , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data Post = Post
  { createdAt :: Data.Text.Text
  , embed :: Maybe PostEmbedKind
  , entities :: Maybe [Entity]
  , facets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , labels :: Maybe PostLabelsKind
  , langs :: Maybe [Data.Text.Text]
  , reply :: Maybe ReplyRef
  , tags :: Maybe [Data.Text.Text]
  , text :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Post where
  parseJSON = Data.Aeson.withObject "Post" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    embed <- v Data.Aeson..:? Data.Aeson.Key.fromString "embed"
    entities <- v Data.Aeson..:? Data.Aeson.Key.fromString "entities"
    facets <- v Data.Aeson..:? Data.Aeson.Key.fromString "facets"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    langs <- v Data.Aeson..:? Data.Aeson.Key.fromString "langs"
    reply <- v Data.Aeson..:? Data.Aeson.Key.fromString "reply"
    tags <- v Data.Aeson..:? Data.Aeson.Key.fromString "tags"
    text <- v Data.Aeson..: Data.Aeson.Key.fromString "text"
    pure $ Post createdAt embed entities facets labels langs reply tags text

instance Data.Aeson.ToJSON Post where
  toJSON (Post createdAt embed entities facets labels langs reply tags text) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "entities" Data.Aeson..?= entities
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "langs" Data.Aeson..?= langs
        , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
        , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]
  toEncoding (Post createdAt embed entities facets labels langs reply tags text) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "entities" Data.Aeson..?= entities
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "langs" Data.Aeson..?= langs
        , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
        , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]

post'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Post -> kv
post'AesonFields (Post createdAt embed entities facets labels langs reply tags text) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
    , Data.Aeson.Key.fromString "entities" Data.Aeson..?= entities
    , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "langs" Data.Aeson..?= langs
    , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
    , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
    , Data.Aeson.Key.fromString "text" Data.Aeson..= text
    ]

data ReplyRef = ReplyRef
  { parent :: Com.Atproto.Repo.StrongRef.StrongRef
  , root :: Com.Atproto.Repo.StrongRef.StrongRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReplyRef where
  parseJSON = Data.Aeson.withObject "ReplyRef" $ \v -> do
    parent <- v Data.Aeson..: Data.Aeson.Key.fromString "parent"
    root <- v Data.Aeson..: Data.Aeson.Key.fromString "root"
    pure $ ReplyRef parent root

instance Data.Aeson.ToJSON ReplyRef where
  toJSON (ReplyRef parent root) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
        , Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]
  toEncoding (ReplyRef parent root) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
        , Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]

replyRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReplyRef -> kv
replyRef'AesonFields (ReplyRef parent root) =
  mconcat
    [ Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
    , Data.Aeson.Key.fromString "root" Data.Aeson..= root
    ]

data TextSlice = TextSlice
  { end :: Integer
  , start :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON TextSlice where
  parseJSON = Data.Aeson.withObject "TextSlice" $ \v -> do
    end <- v Data.Aeson..: Data.Aeson.Key.fromString "end"
    start <- v Data.Aeson..: Data.Aeson.Key.fromString "start"
    pure $ TextSlice end start

instance Data.Aeson.ToJSON TextSlice where
  toJSON (TextSlice end start) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "end" Data.Aeson..= end
        , Data.Aeson.Key.fromString "start" Data.Aeson..= start
        ]
  toEncoding (TextSlice end start) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "end" Data.Aeson..= end
        , Data.Aeson.Key.fromString "start" Data.Aeson..= start
        ]

textSlice'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => TextSlice -> kv
textSlice'AesonFields (TextSlice end start) =
  mconcat
    [ Data.Aeson.Key.fromString "end" Data.Aeson..= end
    , Data.Aeson.Key.fromString "start" Data.Aeson..= start
    ]

data PostEmbedKind
  = PostEmbedKindImages App.Bsky.Embed.Images.Images
  | PostEmbedKindVideo App.Bsky.Embed.Video.Video
  | PostEmbedKindExternal App.Bsky.Embed.External.External
  | PostEmbedKindRecord App.Bsky.Embed.Record.Record
  | PostEmbedKindRecordWithMedia App.Bsky.Embed.RecordWithMedia.RecordWithMedia
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PostEmbedKind where
  parseJSON = Data.Aeson.withObject "PostEmbedKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.images#images" -> PostEmbedKindImages <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.video#video" -> PostEmbedKindVideo <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.external#external" -> PostEmbedKindExternal <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#record" -> PostEmbedKindRecord <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.recordWithMedia#recordWithMedia" -> PostEmbedKindRecordWithMedia <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON PostEmbedKind where
  toJSON = \case
    PostEmbedKindImages v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#images") <> (App.Bsky.Embed.Images.images'AesonFields v))
    PostEmbedKindVideo v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#video") <> (App.Bsky.Embed.Video.video'AesonFields v))
    PostEmbedKindExternal v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#external") <> (App.Bsky.Embed.External.external'AesonFields v))
    PostEmbedKindRecord v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#record") <> (App.Bsky.Embed.Record.record'AesonFields v))
    PostEmbedKindRecordWithMedia v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#recordWithMedia") <> (App.Bsky.Embed.RecordWithMedia.recordWithMedia'AesonFields v))
  toEncoding = \case
    PostEmbedKindImages v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#images") <> (App.Bsky.Embed.Images.images'AesonFields v))
    PostEmbedKindVideo v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#video") <> (App.Bsky.Embed.Video.video'AesonFields v))
    PostEmbedKindExternal v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#external") <> (App.Bsky.Embed.External.external'AesonFields v))
    PostEmbedKindRecord v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#record") <> (App.Bsky.Embed.Record.record'AesonFields v))
    PostEmbedKindRecordWithMedia v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#recordWithMedia") <> (App.Bsky.Embed.RecordWithMedia.recordWithMedia'AesonFields v))

data PostLabelsKind
  = PostLabelsKindSelfLabels Com.Atproto.Label.Defs.SelfLabels
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PostLabelsKind where
  parseJSON = Data.Aeson.withObject "PostLabelsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.label.defs#selfLabels" -> PostLabelsKindSelfLabels <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON PostLabelsKind where
  toJSON = \case
    PostLabelsKindSelfLabels v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
  toEncoding = \case
    PostLabelsKindSelfLabels v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
