{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Moderation.GetMessageContext where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetMessageContextResult = GetMessageContextResult
  { messages :: [GetMessageContextResultMessagesKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetMessageContextResult where
  parseJSON = Data.Aeson.withObject "GetMessageContextResult" $ \v -> do
    messages <- v Data.Aeson..: Data.Aeson.Key.fromString "messages"
    pure $ GetMessageContextResult messages

instance Data.Aeson.ToJSON GetMessageContextResult where
  toJSON (GetMessageContextResult messages) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
        ]
  toEncoding (GetMessageContextResult messages) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
        ]

getMessageContextResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMessageContextResult -> kv
getMessageContextResult'AesonFields (GetMessageContextResult messages) =
  mconcat
    [ Data.Aeson.Key.fromString "messages" Data.Aeson..= messages
    ]

type GetMessageContext = "chat.bsky.moderation.getMessageContext" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "after" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "before" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "convoId" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "messageId" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetMessageContextResult
data GetMessageContextResultMessagesKind
  = GetMessageContextResultMessagesKindMessageView Chat.Bsky.Convo.Defs.MessageView
  | GetMessageContextResultMessagesKindDeletedMessageView Chat.Bsky.Convo.Defs.DeletedMessageView
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetMessageContextResultMessagesKind where
  parseJSON = Data.Aeson.withObject "GetMessageContextResultMessagesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#messageView" -> GetMessageContextResultMessagesKindMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#deletedMessageView" -> GetMessageContextResultMessagesKindDeletedMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetMessageContextResultMessagesKind where
  toJSON = \case
    GetMessageContextResultMessagesKindMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    GetMessageContextResultMessagesKindDeletedMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
  toEncoding = \case
    GetMessageContextResultMessagesKindMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    GetMessageContextResultMessagesKindDeletedMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
