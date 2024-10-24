{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.GetSession where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetSessionResult = GetSessionResult
  { active :: Maybe Bool
  , did :: Data.Text.Text
  , didDoc :: Maybe Data.Aeson.Value
  , email :: Maybe Data.Text.Text
  , emailAuthFactor :: Maybe Bool
  , emailConfirmed :: Maybe Bool
  , handle :: Data.Text.Text
  , status :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSessionResult where
  parseJSON = Data.Aeson.withObject "GetSessionResult" $ \v -> do
    active <- v Data.Aeson..:? Data.Aeson.Key.fromString "active"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    didDoc <- v Data.Aeson..:? Data.Aeson.Key.fromString "didDoc"
    email <- v Data.Aeson..:? Data.Aeson.Key.fromString "email"
    emailAuthFactor <- v Data.Aeson..:? Data.Aeson.Key.fromString "emailAuthFactor"
    emailConfirmed <- v Data.Aeson..:? Data.Aeson.Key.fromString "emailConfirmed"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    status <- v Data.Aeson..:? Data.Aeson.Key.fromString "status"
    pure $ GetSessionResult active did didDoc email emailAuthFactor emailConfirmed handle status

instance Data.Aeson.ToJSON GetSessionResult where
  toJSON (GetSessionResult active did didDoc email emailAuthFactor emailConfirmed handle status) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "didDoc" Data.Aeson..?= didDoc
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailAuthFactor" Data.Aeson..?= emailAuthFactor
        , Data.Aeson.Key.fromString "emailConfirmed" Data.Aeson..?= emailConfirmed
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]
  toEncoding (GetSessionResult active did didDoc email emailAuthFactor emailConfirmed handle status) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "didDoc" Data.Aeson..?= didDoc
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailAuthFactor" Data.Aeson..?= emailAuthFactor
        , Data.Aeson.Key.fromString "emailConfirmed" Data.Aeson..?= emailConfirmed
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]

getSessionResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSessionResult -> kv
getSessionResult'AesonFields (GetSessionResult active did didDoc email emailAuthFactor emailConfirmed handle status) =
  mconcat
    [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "didDoc" Data.Aeson..?= didDoc
    , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
    , Data.Aeson.Key.fromString "emailAuthFactor" Data.Aeson..?= emailAuthFactor
    , Data.Aeson.Key.fromString "emailConfirmed" Data.Aeson..?= emailConfirmed
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
    ]

type GetSession = "com.atproto.server.getSession" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetSessionResult
