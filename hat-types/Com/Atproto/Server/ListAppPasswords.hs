{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.ListAppPasswords where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AppPassword = AppPassword
  { createdAt :: Data.Text.Text
  , name :: Data.Text.Text
  , privileged :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AppPassword where
  parseJSON = Data.Aeson.withObject "AppPassword" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    privileged <- v Data.Aeson..:? Data.Aeson.Key.fromString "privileged"
    pure $ AppPassword createdAt name privileged

instance Data.Aeson.ToJSON AppPassword where
  toJSON (AppPassword createdAt name privileged) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
        ]
  toEncoding (AppPassword createdAt name privileged) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
        ]

appPassword'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AppPassword -> kv
appPassword'AesonFields (AppPassword createdAt name privileged) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
    ]

data ListAppPasswordsResult = ListAppPasswordsResult
  { passwords :: [AppPassword]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListAppPasswordsResult where
  parseJSON = Data.Aeson.withObject "ListAppPasswordsResult" $ \v -> do
    passwords <- v Data.Aeson..: Data.Aeson.Key.fromString "passwords"
    pure $ ListAppPasswordsResult passwords

instance Data.Aeson.ToJSON ListAppPasswordsResult where
  toJSON (ListAppPasswordsResult passwords) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "passwords" Data.Aeson..= passwords
        ]
  toEncoding (ListAppPasswordsResult passwords) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "passwords" Data.Aeson..= passwords
        ]

listAppPasswordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListAppPasswordsResult -> kv
listAppPasswordsResult'AesonFields (ListAppPasswordsResult passwords) =
  mconcat
    [ Data.Aeson.Key.fromString "passwords" Data.Aeson..= passwords
    ]

type ListAppPasswords = "com.atproto.server.listAppPasswords" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListAppPasswordsResult
