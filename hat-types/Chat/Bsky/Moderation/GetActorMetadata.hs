{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Moderation.GetActorMetadata where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetActorMetadataResult = GetActorMetadataResult
  { all :: Metadata
  , day :: Metadata
  , month :: Metadata
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetActorMetadataResult where
  parseJSON = Data.Aeson.withObject "GetActorMetadataResult" $ \v -> do
    all <- v Data.Aeson..: Data.Aeson.Key.fromString "all"
    day <- v Data.Aeson..: Data.Aeson.Key.fromString "day"
    month <- v Data.Aeson..: Data.Aeson.Key.fromString "month"
    pure $ GetActorMetadataResult all day month

instance Data.Aeson.ToJSON GetActorMetadataResult where
  toJSON (GetActorMetadataResult all day month) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "all" Data.Aeson..= all
        , Data.Aeson.Key.fromString "day" Data.Aeson..= day
        , Data.Aeson.Key.fromString "month" Data.Aeson..= month
        ]
  toEncoding (GetActorMetadataResult all day month) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "all" Data.Aeson..= all
        , Data.Aeson.Key.fromString "day" Data.Aeson..= day
        , Data.Aeson.Key.fromString "month" Data.Aeson..= month
        ]

getActorMetadataResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorMetadataResult -> kv
getActorMetadataResult'AesonFields (GetActorMetadataResult all day month) =
  mconcat
    [ Data.Aeson.Key.fromString "all" Data.Aeson..= all
    , Data.Aeson.Key.fromString "day" Data.Aeson..= day
    , Data.Aeson.Key.fromString "month" Data.Aeson..= month
    ]

type GetActorMetadata = "chat.bsky.moderation.getActorMetadata" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetActorMetadataResult
data Metadata = Metadata
  { convos :: Integer
  , convosStarted :: Integer
  , messagesReceived :: Integer
  , messagesSent :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Metadata where
  parseJSON = Data.Aeson.withObject "Metadata" $ \v -> do
    convos <- v Data.Aeson..: Data.Aeson.Key.fromString "convos"
    convosStarted <- v Data.Aeson..: Data.Aeson.Key.fromString "convosStarted"
    messagesReceived <- v Data.Aeson..: Data.Aeson.Key.fromString "messagesReceived"
    messagesSent <- v Data.Aeson..: Data.Aeson.Key.fromString "messagesSent"
    pure $ Metadata convos convosStarted messagesReceived messagesSent

instance Data.Aeson.ToJSON Metadata where
  toJSON (Metadata convos convosStarted messagesReceived messagesSent) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
        , Data.Aeson.Key.fromString "convosStarted" Data.Aeson..= convosStarted
        , Data.Aeson.Key.fromString "messagesReceived" Data.Aeson..= messagesReceived
        , Data.Aeson.Key.fromString "messagesSent" Data.Aeson..= messagesSent
        ]
  toEncoding (Metadata convos convosStarted messagesReceived messagesSent) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
        , Data.Aeson.Key.fromString "convosStarted" Data.Aeson..= convosStarted
        , Data.Aeson.Key.fromString "messagesReceived" Data.Aeson..= messagesReceived
        , Data.Aeson.Key.fromString "messagesSent" Data.Aeson..= messagesSent
        ]

metadata'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Metadata -> kv
metadata'AesonFields (Metadata convos convosStarted messagesReceived messagesSent) =
  mconcat
    [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
    , Data.Aeson.Key.fromString "convosStarted" Data.Aeson..= convosStarted
    , Data.Aeson.Key.fromString "messagesReceived" Data.Aeson..= messagesReceived
    , Data.Aeson.Key.fromString "messagesSent" Data.Aeson..= messagesSent
    ]
