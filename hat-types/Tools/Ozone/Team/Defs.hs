{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Team.Defs where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Member = Member
  { createdAt :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , disabled :: Maybe Bool
  , lastUpdatedBy :: Maybe Data.Text.Text
  , profile :: Maybe App.Bsky.Actor.Defs.ProfileViewDetailed
  , role :: Data.Text.Text
  , updatedAt :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Member where
  parseJSON = Data.Aeson.withObject "Member" $ \v -> do
    createdAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "createdAt"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    disabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "disabled"
    lastUpdatedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastUpdatedBy"
    profile <- v Data.Aeson..:? Data.Aeson.Key.fromString "profile"
    role <- v Data.Aeson..: Data.Aeson.Key.fromString "role"
    updatedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "updatedAt"
    pure $ Member createdAt did disabled lastUpdatedBy profile role updatedAt

instance Data.Aeson.ToJSON Member where
  toJSON (Member createdAt did disabled lastUpdatedBy profile role updatedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..?= disabled
        , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..?= lastUpdatedBy
        , Data.Aeson.Key.fromString "profile" Data.Aeson..?= profile
        , Data.Aeson.Key.fromString "role" Data.Aeson..= role
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..?= updatedAt
        ]
  toEncoding (Member createdAt did disabled lastUpdatedBy profile role updatedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..?= disabled
        , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..?= lastUpdatedBy
        , Data.Aeson.Key.fromString "profile" Data.Aeson..?= profile
        , Data.Aeson.Key.fromString "role" Data.Aeson..= role
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..?= updatedAt
        ]

member'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Member -> kv
member'AesonFields (Member createdAt did disabled lastUpdatedBy profile role updatedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "disabled" Data.Aeson..?= disabled
    , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..?= lastUpdatedBy
    , Data.Aeson.Key.fromString "profile" Data.Aeson..?= profile
    , Data.Aeson.Key.fromString "role" Data.Aeson..= role
    , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..?= updatedAt
    ]
