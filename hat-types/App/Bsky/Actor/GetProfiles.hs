{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.GetProfiles where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetProfilesResult = GetProfilesResult
  { profiles :: [App.Bsky.Actor.Defs.ProfileViewDetailed]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetProfilesResult where
  parseJSON = Data.Aeson.withObject "GetProfilesResult" $ \v -> do
    profiles <- v Data.Aeson..: Data.Aeson.Key.fromString "profiles"
    pure $ GetProfilesResult profiles

instance Data.Aeson.ToJSON GetProfilesResult where
  toJSON (GetProfilesResult profiles) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "profiles" Data.Aeson..= profiles
        ]
  toEncoding (GetProfilesResult profiles) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "profiles" Data.Aeson..= profiles
        ]

getProfilesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetProfilesResult -> kv
getProfilesResult'AesonFields (GetProfilesResult profiles) =
  mconcat
    [ Data.Aeson.Key.fromString "profiles" Data.Aeson..= profiles
    ]

type GetProfiles = "app.bsky.actor.getProfiles" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actors" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetProfilesResult
