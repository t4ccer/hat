{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.GetAccountInviteCodes where

import {-# SOURCE #-} qualified Com.Atproto.Server.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetAccountInviteCodesResult = GetAccountInviteCodesResult
  { codes :: [Com.Atproto.Server.Defs.InviteCode]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetAccountInviteCodesResult where
  parseJSON = Data.Aeson.withObject "GetAccountInviteCodesResult" $ \v -> do
    codes <- v Data.Aeson..: Data.Aeson.Key.fromString "codes"
    pure $ GetAccountInviteCodesResult codes

instance Data.Aeson.ToJSON GetAccountInviteCodesResult where
  toJSON (GetAccountInviteCodesResult codes) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        ]
  toEncoding (GetAccountInviteCodesResult codes) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        ]

getAccountInviteCodesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAccountInviteCodesResult -> kv
getAccountInviteCodesResult'AesonFields (GetAccountInviteCodesResult codes) =
  mconcat
    [ Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
    ]

type GetAccountInviteCodes = "com.atproto.server.getAccountInviteCodes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "createAvailable" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "includeUsed" Bool Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetAccountInviteCodesResult
