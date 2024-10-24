{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Admin.Defs where

import {-# SOURCE #-} qualified Com.Atproto.Server.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AccountView = AccountView
  { deactivatedAt :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , email :: Maybe Data.Text.Text
  , emailConfirmedAt :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , inviteNote :: Maybe Data.Text.Text
  , invitedBy :: Maybe Com.Atproto.Server.Defs.InviteCode
  , invites :: Maybe [Com.Atproto.Server.Defs.InviteCode]
  , invitesDisabled :: Maybe Bool
  , relatedRecords :: Maybe [Data.Aeson.Value]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AccountView where
  parseJSON = Data.Aeson.withObject "AccountView" $ \v -> do
    deactivatedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "deactivatedAt"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    email <- v Data.Aeson..:? Data.Aeson.Key.fromString "email"
    emailConfirmedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "emailConfirmedAt"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    inviteNote <- v Data.Aeson..:? Data.Aeson.Key.fromString "inviteNote"
    invitedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitedBy"
    invites <- v Data.Aeson..:? Data.Aeson.Key.fromString "invites"
    invitesDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitesDisabled"
    relatedRecords <- v Data.Aeson..:? Data.Aeson.Key.fromString "relatedRecords"
    pure $ AccountView deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled relatedRecords

instance Data.Aeson.ToJSON AccountView where
  toJSON (AccountView deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled relatedRecords) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..?= relatedRecords
        ]
  toEncoding (AccountView deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled relatedRecords) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..?= relatedRecords
        ]

accountView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AccountView -> kv
accountView'AesonFields (AccountView deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled relatedRecords) =
  mconcat
    [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
    , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
    , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
    , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
    , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
    , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..?= relatedRecords
    ]

data RepoBlobRef = RepoBlobRef
  { cid :: Data.Text.Text
  , did :: Data.Text.Text
  , recordUri :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoBlobRef where
  parseJSON = Data.Aeson.withObject "RepoBlobRef" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    recordUri <- v Data.Aeson..:? Data.Aeson.Key.fromString "recordUri"
    pure $ RepoBlobRef cid did recordUri

instance Data.Aeson.ToJSON RepoBlobRef where
  toJSON (RepoBlobRef cid did recordUri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "recordUri" Data.Aeson..?= recordUri
        ]
  toEncoding (RepoBlobRef cid did recordUri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "recordUri" Data.Aeson..?= recordUri
        ]

repoBlobRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoBlobRef -> kv
repoBlobRef'AesonFields (RepoBlobRef cid did recordUri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "recordUri" Data.Aeson..?= recordUri
    ]

data RepoRef = RepoRef
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoRef where
  parseJSON = Data.Aeson.withObject "RepoRef" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ RepoRef did

instance Data.Aeson.ToJSON RepoRef where
  toJSON (RepoRef did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (RepoRef did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

repoRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoRef -> kv
repoRef'AesonFields (RepoRef did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

data StatusAttr = StatusAttr
  { applied :: Bool
  , ref :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON StatusAttr where
  parseJSON = Data.Aeson.withObject "StatusAttr" $ \v -> do
    applied <- v Data.Aeson..: Data.Aeson.Key.fromString "applied"
    ref <- v Data.Aeson..:? Data.Aeson.Key.fromString "ref"
    pure $ StatusAttr applied ref

instance Data.Aeson.ToJSON StatusAttr where
  toJSON (StatusAttr applied ref) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "applied" Data.Aeson..= applied
        , Data.Aeson.Key.fromString "ref" Data.Aeson..?= ref
        ]
  toEncoding (StatusAttr applied ref) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "applied" Data.Aeson..= applied
        , Data.Aeson.Key.fromString "ref" Data.Aeson..?= ref
        ]

statusAttr'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StatusAttr -> kv
statusAttr'AesonFields (StatusAttr applied ref) =
  mconcat
    [ Data.Aeson.Key.fromString "applied" Data.Aeson..= applied
    , Data.Aeson.Key.fromString "ref" Data.Aeson..?= ref
    ]
