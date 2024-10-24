{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.Profile where

import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Profile = Profile
  { avatar :: Maybe Data.ByteString.ByteString
  , banner :: Maybe Data.ByteString.ByteString
  , createdAt :: Maybe Data.Text.Text
  , description :: Maybe Data.Text.Text
  , displayName :: Maybe Data.Text.Text
  , joinedViaStarterPack :: Maybe Com.Atproto.Repo.StrongRef.StrongRef
  , labels :: Maybe ProfileLabelsKind
  , pinnedPost :: Maybe Com.Atproto.Repo.StrongRef.StrongRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Profile where
  parseJSON = Data.Aeson.withObject "Profile" $ \v -> do
    avatar <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    banner <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "banner"
    createdAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    displayName <- v Data.Aeson..:? Data.Aeson.Key.fromString "displayName"
    joinedViaStarterPack <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedViaStarterPack"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    pinnedPost <- v Data.Aeson..:? Data.Aeson.Key.fromString "pinnedPost"
    pure $ Profile avatar banner createdAt description displayName joinedViaStarterPack labels pinnedPost

instance Data.Aeson.ToJSON Profile where
  toJSON (Profile avatar banner createdAt description displayName joinedViaStarterPack labels pinnedPost) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "banner" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 banner
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
        ]
  toEncoding (Profile avatar banner createdAt description displayName joinedViaStarterPack labels pinnedPost) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
        , Data.Aeson.Key.fromString "banner" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 banner
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
        ]

profile'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Profile -> kv
profile'AesonFields (Profile avatar banner createdAt description displayName joinedViaStarterPack labels pinnedPost) =
  mconcat
    [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 avatar
    , Data.Aeson.Key.fromString "banner" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 banner
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
    , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
    ]

data ProfileLabelsKind
  = ProfileLabelsKindSelfLabels Com.Atproto.Label.Defs.SelfLabels
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileLabelsKind where
  parseJSON = Data.Aeson.withObject "ProfileLabelsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.label.defs#selfLabels" -> ProfileLabelsKindSelfLabels <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ProfileLabelsKind where
  toJSON = \case
    ProfileLabelsKindSelfLabels v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
  toEncoding = \case
    ProfileLabelsKindSelfLabels v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.label.defs#selfLabels") <> (Com.Atproto.Label.Defs.selfLabels'AesonFields v))
