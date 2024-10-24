{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.GetServiceAuth where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetServiceAuthResult = GetServiceAuthResult
  { token :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetServiceAuthResult where
  parseJSON = Data.Aeson.withObject "GetServiceAuthResult" $ \v -> do
    token <- v Data.Aeson..: Data.Aeson.Key.fromString "token"
    pure $ GetServiceAuthResult token

instance Data.Aeson.ToJSON GetServiceAuthResult where
  toJSON (GetServiceAuthResult token) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "token" Data.Aeson..= token
        ]
  toEncoding (GetServiceAuthResult token) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "token" Data.Aeson..= token
        ]

getServiceAuthResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetServiceAuthResult -> kv
getServiceAuthResult'AesonFields (GetServiceAuthResult token) =
  mconcat
    [ Data.Aeson.Key.fromString "token" Data.Aeson..= token
    ]

type GetServiceAuth = "com.atproto.server.getServiceAuth" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "aud" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "exp" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "lxm" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetServiceAuthResult
