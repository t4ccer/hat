{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.GetMessages where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetMessagesResult = GetMessagesResult
  { cursor :: Maybe Data.Text.Text
  , messages :: [GetMessagesResultMessagesKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetMessagesResult where
  parseJSON = Data.Aeson.withObject "GetMessagesResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    messages <- v Data.Aeson..: Data.Aeson.Key.fromString "messages"
    pure $ GetMessagesResult cursor messages

instance Data.Aeson.ToJSON GetMessagesResult where
  toJSON (GetMessagesResult cursor messages) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
        ]
  toEncoding (GetMessagesResult cursor messages) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
        ]

getMessagesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMessagesResult -> kv
getMessagesResult'AesonFields (GetMessagesResult cursor messages) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
    ]

type GetMessages = "chat.bsky.convo.getMessages" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "convoId" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetMessagesResult
data GetMessagesResultMessagesKind
  = GetMessagesResultMessagesKindMessageView Chat.Bsky.Convo.Defs.MessageView
  | GetMessagesResultMessagesKindDeletedMessageView Chat.Bsky.Convo.Defs.DeletedMessageView
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetMessagesResultMessagesKind where
  parseJSON = Data.Aeson.withObject "GetMessagesResultMessagesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#messageView" -> GetMessagesResultMessagesKindMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#deletedMessageView" -> GetMessagesResultMessagesKindDeletedMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetMessagesResultMessagesKind where
  toJSON = \case
    GetMessagesResultMessagesKindMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    GetMessagesResultMessagesKindDeletedMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
  toEncoding = \case
    GetMessagesResultMessagesKindMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    GetMessagesResultMessagesKindDeletedMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
