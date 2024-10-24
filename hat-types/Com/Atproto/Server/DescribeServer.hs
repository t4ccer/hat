{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.DescribeServer where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Contact = Contact
  { email :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Contact where
  parseJSON = Data.Aeson.withObject "Contact" $ \v -> do
    email <- v Data.Aeson..:? Data.Aeson.Key.fromString "email"
    pure $ Contact email

instance Data.Aeson.ToJSON Contact where
  toJSON (Contact email) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        ]
  toEncoding (Contact email) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        ]

contact'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Contact -> kv
contact'AesonFields (Contact email) =
  mconcat
    [ Data.Aeson.Key.fromString "email" Data.Aeson..?= email
    ]

data Links = Links
  { privacyPolicy :: Maybe Data.Text.Text
  , termsOfService :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Links where
  parseJSON = Data.Aeson.withObject "Links" $ \v -> do
    privacyPolicy <- v Data.Aeson..:? Data.Aeson.Key.fromString "privacyPolicy"
    termsOfService <- v Data.Aeson..:? Data.Aeson.Key.fromString "termsOfService"
    pure $ Links privacyPolicy termsOfService

instance Data.Aeson.ToJSON Links where
  toJSON (Links privacyPolicy termsOfService) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
        , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
        ]
  toEncoding (Links privacyPolicy termsOfService) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
        , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
        ]

links'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Links -> kv
links'AesonFields (Links privacyPolicy termsOfService) =
  mconcat
    [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
    , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
    ]

data DescribeServerResult = DescribeServerResult
  { availableUserDomains :: [Data.Text.Text]
  , contact :: Maybe Contact
  , did :: Data.Text.Text
  , inviteCodeRequired :: Maybe Bool
  , links :: Maybe Links
  , phoneVerificationRequired :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DescribeServerResult where
  parseJSON = Data.Aeson.withObject "DescribeServerResult" $ \v -> do
    availableUserDomains <- v Data.Aeson..: Data.Aeson.Key.fromString "availableUserDomains"
    contact <- v Data.Aeson..:? Data.Aeson.Key.fromString "contact"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    inviteCodeRequired <- v Data.Aeson..:? Data.Aeson.Key.fromString "inviteCodeRequired"
    links <- v Data.Aeson..:? Data.Aeson.Key.fromString "links"
    phoneVerificationRequired <- v Data.Aeson..:? Data.Aeson.Key.fromString "phoneVerificationRequired"
    pure $ DescribeServerResult availableUserDomains contact did inviteCodeRequired links phoneVerificationRequired

instance Data.Aeson.ToJSON DescribeServerResult where
  toJSON (DescribeServerResult availableUserDomains contact did inviteCodeRequired links phoneVerificationRequired) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "availableUserDomains" Data.Aeson..= availableUserDomains
        , Data.Aeson.Key.fromString "contact" Data.Aeson..?= contact
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "inviteCodeRequired" Data.Aeson..?= inviteCodeRequired
        , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
        , Data.Aeson.Key.fromString "phoneVerificationRequired" Data.Aeson..?= phoneVerificationRequired
        ]
  toEncoding (DescribeServerResult availableUserDomains contact did inviteCodeRequired links phoneVerificationRequired) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "availableUserDomains" Data.Aeson..= availableUserDomains
        , Data.Aeson.Key.fromString "contact" Data.Aeson..?= contact
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "inviteCodeRequired" Data.Aeson..?= inviteCodeRequired
        , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
        , Data.Aeson.Key.fromString "phoneVerificationRequired" Data.Aeson..?= phoneVerificationRequired
        ]

describeServerResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeServerResult -> kv
describeServerResult'AesonFields (DescribeServerResult availableUserDomains contact did inviteCodeRequired links phoneVerificationRequired) =
  mconcat
    [ Data.Aeson.Key.fromString "availableUserDomains" Data.Aeson..= availableUserDomains
    , Data.Aeson.Key.fromString "contact" Data.Aeson..?= contact
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "inviteCodeRequired" Data.Aeson..?= inviteCodeRequired
    , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
    , Data.Aeson.Key.fromString "phoneVerificationRequired" Data.Aeson..?= phoneVerificationRequired
    ]

type DescribeServer = "com.atproto.server.describeServer" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] DescribeServerResult
