{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Signature.SearchAccounts where

import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchAccountsResult = SearchAccountsResult
  { accounts :: [Com.Atproto.Admin.Defs.AccountView]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchAccountsResult where
  parseJSON = Data.Aeson.withObject "SearchAccountsResult" $ \v -> do
    accounts <- v Data.Aeson..: Data.Aeson.Key.fromString "accounts"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ SearchAccountsResult accounts cursor

instance Data.Aeson.ToJSON SearchAccountsResult where
  toJSON (SearchAccountsResult accounts cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (SearchAccountsResult accounts cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

searchAccountsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchAccountsResult -> kv
searchAccountsResult'AesonFields (SearchAccountsResult accounts cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type SearchAccounts = "tools.ozone.signature.searchAccounts" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "values" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchAccountsResult
