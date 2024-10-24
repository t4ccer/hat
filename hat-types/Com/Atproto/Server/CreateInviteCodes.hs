{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.CreateInviteCodes where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AccountCodes = AccountCodes
  { account :: Data.Text.Text
  , codes :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AccountCodes where
  parseJSON = Data.Aeson.withObject "AccountCodes" $ \v -> do
    account <- v Data.Aeson..: Data.Aeson.Key.fromString "account"
    codes <- v Data.Aeson..: Data.Aeson.Key.fromString "codes"
    pure $ AccountCodes account codes

instance Data.Aeson.ToJSON AccountCodes where
  toJSON (AccountCodes account codes) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
        , Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        ]
  toEncoding (AccountCodes account codes) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
        , Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
        ]

accountCodes'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AccountCodes -> kv
accountCodes'AesonFields (AccountCodes account codes) =
  mconcat
    [ Data.Aeson.Key.fromString "account" Data.Aeson..= account
    , Data.Aeson.Key.fromString "codes" Data.Aeson..= codes
    ]
