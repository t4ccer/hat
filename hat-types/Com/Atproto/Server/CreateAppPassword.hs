{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.CreateAppPassword where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AppPassword = AppPassword
  { createdAt :: Data.Text.Text
  , name :: Data.Text.Text
  , password :: Data.Text.Text
  , privileged :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AppPassword where
  parseJSON = Data.Aeson.withObject "AppPassword" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    password <- v Data.Aeson..: Data.Aeson.Key.fromString "password"
    privileged <- v Data.Aeson..:? Data.Aeson.Key.fromString "privileged"
    pure $ AppPassword createdAt name password privileged

instance Data.Aeson.ToJSON AppPassword where
  toJSON (AppPassword createdAt name password privileged) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "password" Data.Aeson..= password
        , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
        ]
  toEncoding (AppPassword createdAt name password privileged) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "password" Data.Aeson..= password
        , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
        ]

appPassword'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AppPassword -> kv
appPassword'AesonFields (AppPassword createdAt name password privileged) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "password" Data.Aeson..= password
    , Data.Aeson.Key.fromString "privileged" Data.Aeson..?= privileged
    ]
