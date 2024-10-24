{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data InviteCode = InviteCode
  { available :: Integer
  , code :: Data.Text.Text
  , createdAt :: Data.Text.Text
  , createdBy :: Data.Text.Text
  , disabled :: Bool
  , forAccount :: Data.Text.Text
  , uses :: [InviteCodeUse]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON InviteCode where
  parseJSON = Data.Aeson.withObject "InviteCode" $ \v -> do
    available <- v Data.Aeson..: Data.Aeson.Key.fromString "available"
    code <- v Data.Aeson..: Data.Aeson.Key.fromString "code"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    createdBy <- v Data.Aeson..: Data.Aeson.Key.fromString "createdBy"
    disabled <- v Data.Aeson..: Data.Aeson.Key.fromString "disabled"
    forAccount <- v Data.Aeson..: Data.Aeson.Key.fromString "forAccount"
    uses <- v Data.Aeson..: Data.Aeson.Key.fromString "uses"
    pure $ InviteCode available code createdAt createdBy disabled forAccount uses

instance Data.Aeson.ToJSON InviteCode where
  toJSON (InviteCode available code createdAt createdBy disabled forAccount uses) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "available" Data.Aeson..= available
        , Data.Aeson.Key.fromString "code" Data.Aeson..= code
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
        , Data.Aeson.Key.fromString "forAccount" Data.Aeson..= forAccount
        , Data.Aeson.Key.fromString "uses" Data.Aeson..= uses
        ]
  toEncoding (InviteCode available code createdAt createdBy disabled forAccount uses) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "available" Data.Aeson..= available
        , Data.Aeson.Key.fromString "code" Data.Aeson..= code
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
        , Data.Aeson.Key.fromString "forAccount" Data.Aeson..= forAccount
        , Data.Aeson.Key.fromString "uses" Data.Aeson..= uses
        ]

inviteCode'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InviteCode -> kv
inviteCode'AesonFields (InviteCode available code createdAt createdBy disabled forAccount uses) =
  mconcat
    [ Data.Aeson.Key.fromString "available" Data.Aeson..= available
    , Data.Aeson.Key.fromString "code" Data.Aeson..= code
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
    , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
    , Data.Aeson.Key.fromString "forAccount" Data.Aeson..= forAccount
    , Data.Aeson.Key.fromString "uses" Data.Aeson..= uses
    ]

data InviteCodeUse = InviteCodeUse
  { usedAt :: Data.Text.Text
  , usedBy :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON InviteCodeUse where
  parseJSON = Data.Aeson.withObject "InviteCodeUse" $ \v -> do
    usedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "usedAt"
    usedBy <- v Data.Aeson..: Data.Aeson.Key.fromString "usedBy"
    pure $ InviteCodeUse usedAt usedBy

instance Data.Aeson.ToJSON InviteCodeUse where
  toJSON (InviteCodeUse usedAt usedBy) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "usedAt" Data.Aeson..= usedAt
        , Data.Aeson.Key.fromString "usedBy" Data.Aeson..= usedBy
        ]
  toEncoding (InviteCodeUse usedAt usedBy) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "usedAt" Data.Aeson..= usedAt
        , Data.Aeson.Key.fromString "usedBy" Data.Aeson..= usedBy
        ]

inviteCodeUse'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InviteCodeUse -> kv
inviteCodeUse'AesonFields (InviteCodeUse usedAt usedBy) =
  mconcat
    [ Data.Aeson.Key.fromString "usedAt" Data.Aeson..= usedAt
    , Data.Aeson.Key.fromString "usedBy" Data.Aeson..= usedBy
    ]
