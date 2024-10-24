{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Admin.GetAccountInfos where

import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetAccountInfosResult = GetAccountInfosResult
  { infos :: [Com.Atproto.Admin.Defs.AccountView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetAccountInfosResult where
  parseJSON = Data.Aeson.withObject "GetAccountInfosResult" $ \v -> do
    infos <- v Data.Aeson..: Data.Aeson.Key.fromString "infos"
    pure $ GetAccountInfosResult infos

instance Data.Aeson.ToJSON GetAccountInfosResult where
  toJSON (GetAccountInfosResult infos) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "infos" Data.Aeson..= infos
        ]
  toEncoding (GetAccountInfosResult infos) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "infos" Data.Aeson..= infos
        ]

getAccountInfosResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAccountInfosResult -> kv
getAccountInfosResult'AesonFields (GetAccountInfosResult infos) =
  mconcat
    [ Data.Aeson.Key.fromString "infos" Data.Aeson..= infos
    ]

type GetAccountInfos = "com.atproto.admin.getAccountInfos" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "dids" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetAccountInfosResult
