{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Labeler.Service where

import {-# SOURCE #-} qualified App.Bsky.Labeler.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Service = Service
  { createdAt :: Data.Text.Text
  , labels :: Maybe ServiceLabelsKind
  , policies :: App.Bsky.Labeler.Defs.LabelerPolicies
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Service where
  parseJSON = Data.Aeson.withObject "Service" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    policies <- v Data.Aeson..: Data.Aeson.Key.fromString "policies"
    pure $ Service createdAt labels policies

instance Data.Aeson.ToJSON Service where
  toJSON (Service createdAt labels policies) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
        ]
  toEncoding (Service createdAt labels policies) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
        ]

service'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Service -> kv
service'AesonFields (Service createdAt labels policies) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
    ]

data ServiceLabelsKind
  = ServiceLabelsKindSelfLabels Com.Atproto.Label.Defs.SelfLabels
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ServiceLabelsKind where
  parseJSON = Data.Aeson.withObject "ServiceLabelsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.label.defs#selfLabels" -> ServiceLabelsKindSelfLabels <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ServiceLabelsKind where
  toJSON = \case
    ServiceLabelsKindSelfLabels v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
  toEncoding = \case
    ServiceLabelsKindSelfLabels v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
