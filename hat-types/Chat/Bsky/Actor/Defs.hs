{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Actor.Defs where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ProfileViewBasic = ProfileViewBasic
  { associated :: Maybe App.Bsky.Actor.Defs.ProfileAssociated
  , avatar :: Maybe Data.Text.Text
  , chatDisabled :: Maybe Bool
  , did :: Data.Text.Text
  , displayName :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , viewer :: Maybe App.Bsky.Actor.Defs.ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileViewBasic where
  parseJSON = Data.Aeson.withObject "ProfileViewBasic" $ \v -> do
    associated <- v Data.Aeson..:? Data.Aeson.Key.fromString "associated"
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    chatDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "chatDisabled"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..:? Data.Aeson.Key.fromString "displayName"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ProfileViewBasic associated avatar chatDisabled did displayName handle labels viewer

instance Data.Aeson.ToJSON ProfileViewBasic where
  toJSON (ProfileViewBasic associated avatar chatDisabled did displayName handle labels viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "chatDisabled" Data.Aeson..?= chatDisabled
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ProfileViewBasic associated avatar chatDisabled did displayName handle labels viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "chatDisabled" Data.Aeson..?= chatDisabled
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

profileViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewBasic -> kv
profileViewBasic'AesonFields (ProfileViewBasic associated avatar chatDisabled did displayName handle labels viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "chatDisabled" Data.Aeson..?= chatDisabled
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]
