{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Signature.FindRelatedAccounts where

import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Signature.Defs

data FindRelatedAccountsResult = FindRelatedAccountsResult
  { accounts :: [RelatedAccount]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FindRelatedAccountsResult where
  parseJSON = Data.Aeson.withObject "FindRelatedAccountsResult" $ \v -> do
    accounts <- v Data.Aeson..: Data.Aeson.Key.fromString "accounts"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ FindRelatedAccountsResult accounts cursor

instance Data.Aeson.ToJSON FindRelatedAccountsResult where
  toJSON (FindRelatedAccountsResult accounts cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (FindRelatedAccountsResult accounts cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

findRelatedAccountsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FindRelatedAccountsResult -> kv
findRelatedAccountsResult'AesonFields (FindRelatedAccountsResult accounts cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "accounts" Data.Aeson..= accounts
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type FindRelatedAccounts = "tools.ozone.signature.findRelatedAccounts" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] FindRelatedAccountsResult
data RelatedAccount = RelatedAccount
  { account :: Com.Atproto.Admin.Defs.AccountView
  , similarities :: Maybe [Tools.Ozone.Signature.Defs.SigDetail]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RelatedAccount where
  parseJSON = Data.Aeson.withObject "RelatedAccount" $ \v -> do
    account <- v Data.Aeson..: Data.Aeson.Key.fromString "account"
    similarities <- v Data.Aeson..:? Data.Aeson.Key.fromString "similarities"
    pure $ RelatedAccount account similarities

instance Data.Aeson.ToJSON RelatedAccount where
  toJSON (RelatedAccount account similarities) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
        , Data.Aeson.Key.fromString "similarities" Data.Aeson..?= similarities
        ]
  toEncoding (RelatedAccount account similarities) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
        , Data.Aeson.Key.fromString "similarities" Data.Aeson..?= similarities
        ]

relatedAccount'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RelatedAccount -> kv
relatedAccount'AesonFields (RelatedAccount account similarities) =
  mconcat
    [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
    , Data.Aeson.Key.fromString "similarities" Data.Aeson..?= similarities
    ]
