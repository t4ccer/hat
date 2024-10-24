{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Server.GetConfig where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetConfigResult = GetConfigResult
  { appview :: Maybe ServiceConfig
  , blobDivert :: Maybe ServiceConfig
  , chat :: Maybe ServiceConfig
  , pds :: Maybe ServiceConfig
  , viewer :: Maybe ViewerConfig
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetConfigResult where
  parseJSON = Data.Aeson.withObject "GetConfigResult" $ \v -> do
    appview <- v Data.Aeson..:? Data.Aeson.Key.fromString "appview"
    blobDivert <- v Data.Aeson..:? Data.Aeson.Key.fromString "blobDivert"
    chat <- v Data.Aeson..:? Data.Aeson.Key.fromString "chat"
    pds <- v Data.Aeson..:? Data.Aeson.Key.fromString "pds"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ GetConfigResult appview blobDivert chat pds viewer

instance Data.Aeson.ToJSON GetConfigResult where
  toJSON (GetConfigResult appview blobDivert chat pds viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "appview" Data.Aeson..?= appview
        , Data.Aeson.Key.fromString "blobDivert" Data.Aeson..?= blobDivert
        , Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
        , Data.Aeson.Key.fromString "pds" Data.Aeson..?= pds
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (GetConfigResult appview blobDivert chat pds viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "appview" Data.Aeson..?= appview
        , Data.Aeson.Key.fromString "blobDivert" Data.Aeson..?= blobDivert
        , Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
        , Data.Aeson.Key.fromString "pds" Data.Aeson..?= pds
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

getConfigResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConfigResult -> kv
getConfigResult'AesonFields (GetConfigResult appview blobDivert chat pds viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "appview" Data.Aeson..?= appview
    , Data.Aeson.Key.fromString "blobDivert" Data.Aeson..?= blobDivert
    , Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
    , Data.Aeson.Key.fromString "pds" Data.Aeson..?= pds
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

type GetConfig = "tools.ozone.server.getConfig" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetConfigResult
data ServiceConfig = ServiceConfig
  { url :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ServiceConfig where
  parseJSON = Data.Aeson.withObject "ServiceConfig" $ \v -> do
    url <- v Data.Aeson..:? Data.Aeson.Key.fromString "url"
    pure $ ServiceConfig url

instance Data.Aeson.ToJSON ServiceConfig where
  toJSON (ServiceConfig url) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "url" Data.Aeson..?= url
        ]
  toEncoding (ServiceConfig url) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "url" Data.Aeson..?= url
        ]

serviceConfig'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ServiceConfig -> kv
serviceConfig'AesonFields (ServiceConfig url) =
  mconcat
    [ Data.Aeson.Key.fromString "url" Data.Aeson..?= url
    ]

data ViewerConfig = ViewerConfig
  { role :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewerConfig where
  parseJSON = Data.Aeson.withObject "ViewerConfig" $ \v -> do
    role <- v Data.Aeson..:? Data.Aeson.Key.fromString "role"
    pure $ ViewerConfig role

instance Data.Aeson.ToJSON ViewerConfig where
  toJSON (ViewerConfig role) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "role" Data.Aeson..?= role
        ]
  toEncoding (ViewerConfig role) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "role" Data.Aeson..?= role
        ]

viewerConfig'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerConfig -> kv
viewerConfig'AesonFields (ViewerConfig role) =
  mconcat
    [ Data.Aeson.Key.fromString "role" Data.Aeson..?= role
    ]
