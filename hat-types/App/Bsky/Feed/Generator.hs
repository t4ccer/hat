{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Generator where

import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Generator = Generator
  { acceptsInteractions :: Maybe Bool
  , avatar :: Maybe Data.ByteString.ByteString
  , createdAt :: Data.Text.Text
  , description :: Maybe Data.Text.Text
  , descriptionFacets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , did :: Data.Text.Text
  , displayName :: Data.Text.Text
  , labels :: Maybe GeneratorLabelsKind
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Generator where
  parseJSON = Data.Aeson.withObject "Generator" $ \v -> do
    acceptsInteractions <- v Data.Aeson..:? Data.Aeson.Key.fromString "acceptsInteractions"
    avatar <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    descriptionFacets <- v Data.Aeson..:? Data.Aeson.Key.fromString "descriptionFacets"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..: Data.Aeson.Key.fromString "displayName"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    pure $ Generator acceptsInteractions avatar createdAt description descriptionFacets did displayName labels

instance Data.Aeson.ToJSON Generator where
  toJSON (Generator acceptsInteractions avatar createdAt description descriptionFacets did displayName labels) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        ]
  toEncoding (Generator acceptsInteractions avatar createdAt description descriptionFacets did displayName labels) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        ]

generator'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Generator -> kv
generator'AesonFields (Generator acceptsInteractions avatar createdAt description descriptionFacets did displayName labels) =
  mconcat
    [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    ]

data GeneratorLabelsKind
  = GeneratorLabelsKindSelfLabels Com.Atproto.Label.Defs.SelfLabels
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GeneratorLabelsKind where
  parseJSON = Data.Aeson.withObject "GeneratorLabelsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.label.defs#selfLabels" -> GeneratorLabelsKindSelfLabels <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GeneratorLabelsKind where
  toJSON = \case
    GeneratorLabelsKindSelfLabels v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
  toEncoding = \case
    GeneratorLabelsKindSelfLabels v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
