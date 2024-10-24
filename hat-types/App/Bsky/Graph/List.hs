{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.List where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data List = List
  { avatar :: Maybe Data.ByteString.ByteString
  , createdAt :: Data.Text.Text
  , description :: Maybe Data.Text.Text
  , descriptionFacets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , labels :: Maybe ListLabelsKind
  , name :: Data.Text.Text
  , purpose :: App.Bsky.Graph.Defs.ListPurpose
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON List where
  parseJSON = Data.Aeson.withObject "List" $ \v -> do
    avatar <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    descriptionFacets <- v Data.Aeson..:? Data.Aeson.Key.fromString "descriptionFacets"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    purpose <- v Data.Aeson..: Data.Aeson.Key.fromString "purpose"
    pure $ List avatar createdAt description descriptionFacets labels name purpose

instance Data.Aeson.ToJSON List where
  toJSON (List avatar createdAt description descriptionFacets labels name purpose) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        ]
  toEncoding (List avatar createdAt description descriptionFacets labels name purpose) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        ]

list'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => List -> kv
list'AesonFields (List avatar createdAt description descriptionFacets labels name purpose) =
  mconcat
    [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
    ]

data ListLabelsKind
  = ListLabelsKindSelfLabels Com.Atproto.Label.Defs.SelfLabels
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListLabelsKind where
  parseJSON = Data.Aeson.withObject "ListLabelsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.label.defs#selfLabels" -> ListLabelsKindSelfLabels <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ListLabelsKind where
  toJSON = \case
    ListLabelsKindSelfLabels v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
  toEncoding = \case
    ListLabelsKindSelfLabels v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
