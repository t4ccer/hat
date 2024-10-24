{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Labeler.Defs where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data LabelerPolicies = LabelerPolicies
  { labelValueDefinitions :: Maybe [Com.Atproto.Label.Defs.LabelValueDefinition]
  , labelValues :: [Com.Atproto.Label.Defs.LabelValue]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelerPolicies where
  parseJSON = Data.Aeson.withObject "LabelerPolicies" $ \v -> do
    labelValueDefinitions <- v Data.Aeson..:? Data.Aeson.Key.fromString "labelValueDefinitions"
    labelValues <- v Data.Aeson..: Data.Aeson.Key.fromString "labelValues"
    pure $ LabelerPolicies labelValueDefinitions labelValues

instance Data.Aeson.ToJSON LabelerPolicies where
  toJSON (LabelerPolicies labelValueDefinitions labelValues) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "labelValueDefinitions" Data.Aeson..?= labelValueDefinitions
        , Data.Aeson.Key.fromString "labelValues" Data.Aeson..= labelValues
        ]
  toEncoding (LabelerPolicies labelValueDefinitions labelValues) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "labelValueDefinitions" Data.Aeson..?= labelValueDefinitions
        , Data.Aeson.Key.fromString "labelValues" Data.Aeson..= labelValues
        ]

labelerPolicies'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerPolicies -> kv
labelerPolicies'AesonFields (LabelerPolicies labelValueDefinitions labelValues) =
  mconcat
    [ Data.Aeson.Key.fromString "labelValueDefinitions" Data.Aeson..?= labelValueDefinitions
    , Data.Aeson.Key.fromString "labelValues" Data.Aeson..= labelValues
    ]

data LabelerView = LabelerView
  { cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileView
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , likeCount :: Maybe Integer
  , uri :: Data.Text.Text
  , viewer :: Maybe LabelerViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelerView where
  parseJSON = Data.Aeson.withObject "LabelerView" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    likeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "likeCount"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ LabelerView cid creator indexedAt labels likeCount uri viewer

instance Data.Aeson.ToJSON LabelerView where
  toJSON (LabelerView cid creator indexedAt labels likeCount uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (LabelerView cid creator indexedAt labels likeCount uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

labelerView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerView -> kv
labelerView'AesonFields (LabelerView cid creator indexedAt labels likeCount uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data LabelerViewDetailed = LabelerViewDetailed
  { cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileView
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , likeCount :: Maybe Integer
  , policies :: LabelerPolicies
  , uri :: Data.Text.Text
  , viewer :: Maybe LabelerViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelerViewDetailed where
  parseJSON = Data.Aeson.withObject "LabelerViewDetailed" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    likeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "likeCount"
    policies <- v Data.Aeson..: Data.Aeson.Key.fromString "policies"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ LabelerViewDetailed cid creator indexedAt labels likeCount policies uri viewer

instance Data.Aeson.ToJSON LabelerViewDetailed where
  toJSON (LabelerViewDetailed cid creator indexedAt labels likeCount policies uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (LabelerViewDetailed cid creator indexedAt labels likeCount policies uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

labelerViewDetailed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerViewDetailed -> kv
labelerViewDetailed'AesonFields (LabelerViewDetailed cid creator indexedAt labels likeCount policies uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
    , Data.Aeson.Key.fromString "policies" Data.Aeson..= policies
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data LabelerViewerState = LabelerViewerState
  { like :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelerViewerState where
  parseJSON = Data.Aeson.withObject "LabelerViewerState" $ \v -> do
    like <- v Data.Aeson..:? Data.Aeson.Key.fromString "like"
    pure $ LabelerViewerState like

instance Data.Aeson.ToJSON LabelerViewerState where
  toJSON (LabelerViewerState like) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        ]
  toEncoding (LabelerViewerState like) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        ]

labelerViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerViewerState -> kv
labelerViewerState'AesonFields (LabelerViewerState like) =
  mconcat
    [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
    ]
