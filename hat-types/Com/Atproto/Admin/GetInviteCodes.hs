{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Admin.GetInviteCodes where

import {-# SOURCE #-} qualified Com.Atproto.Server.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetInviteCodesResult = GetInviteCodesResult
  { codes :: [Com.Atproto.Server.Defs.InviteCode]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetInviteCodesResult where
  parseJSON = Data.Aeson.withObject "GetInviteCodesResult" $ \v -> do
    codes <- v Data.Aeson..: Data.Aeson.Key.fromString "codes"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ GetInviteCodesResult codes cursor

instance Data.Aeson.ToJSON GetInviteCodesResult where
  toJSON (GetInviteCodesResult codes cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (GetInviteCodesResult codes cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

getInviteCodesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetInviteCodesResult -> kv
getInviteCodesResult'AesonFields (GetInviteCodesResult codes cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type GetInviteCodes = "com.atproto.admin.getInviteCodes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sort" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetInviteCodesResult
