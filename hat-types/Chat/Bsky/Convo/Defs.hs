{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.Defs where

import {-# SOURCE #-} qualified App.Bsky.Embed.Record
import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Chat.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ConvoView = ConvoView
  { id' :: Data.Text.Text
  , lastMessage :: Maybe ConvoViewLastMessageKind
  , members :: [Chat.Bsky.Actor.Defs.ProfileViewBasic]
  , muted :: Bool
  , rev :: Data.Text.Text
  , unreadCount :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ConvoView where
  parseJSON = Data.Aeson.withObject "ConvoView" $ \v -> do
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    lastMessage <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastMessage"
    members <- v Data.Aeson..: Data.Aeson.Key.fromString "members"
    muted <- v Data.Aeson..: Data.Aeson.Key.fromString "muted"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    unreadCount <- v Data.Aeson..: Data.Aeson.Key.fromString "unreadCount"
    pure $ ConvoView id' lastMessage members muted rev unreadCount

instance Data.Aeson.ToJSON ConvoView where
  toJSON (ConvoView id' lastMessage members muted rev unreadCount) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lastMessage" Data.Aeson..?= lastMessage
        , Data.Aeson.Key.fromString "members" Data.Aeson..= members
        , Data.Aeson.Key.fromString "muted" Data.Aeson..= muted
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "unreadCount" Data.Aeson..= unreadCount
        ]
  toEncoding (ConvoView id' lastMessage members muted rev unreadCount) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lastMessage" Data.Aeson..?= lastMessage
        , Data.Aeson.Key.fromString "members" Data.Aeson..= members
        , Data.Aeson.Key.fromString "muted" Data.Aeson..= muted
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "unreadCount" Data.Aeson..= unreadCount
        ]

convoView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ConvoView -> kv
convoView'AesonFields (ConvoView id' lastMessage members muted rev unreadCount) =
  mconcat
    [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "lastMessage" Data.Aeson..?= lastMessage
    , Data.Aeson.Key.fromString "members" Data.Aeson..= members
    , Data.Aeson.Key.fromString "muted" Data.Aeson..= muted
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    , Data.Aeson.Key.fromString "unreadCount" Data.Aeson..= unreadCount
    ]

data DeletedMessageView = DeletedMessageView
  { id' :: Data.Text.Text
  , rev :: Data.Text.Text
  , sender :: MessageViewSender
  , sentAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DeletedMessageView where
  parseJSON = Data.Aeson.withObject "DeletedMessageView" $ \v -> do
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    sender <- v Data.Aeson..: Data.Aeson.Key.fromString "sender"
    sentAt <- v Data.Aeson..: Data.Aeson.Key.fromString "sentAt"
    pure $ DeletedMessageView id' rev sender sentAt

instance Data.Aeson.ToJSON DeletedMessageView where
  toJSON (DeletedMessageView id' rev sender sentAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
        , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
        ]
  toEncoding (DeletedMessageView id' rev sender sentAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
        , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
        ]

deletedMessageView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DeletedMessageView -> kv
deletedMessageView'AesonFields (DeletedMessageView id' rev sender sentAt) =
  mconcat
    [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
    , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
    ]

data LogBeginConvo = LogBeginConvo
  { convoId :: Data.Text.Text
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogBeginConvo where
  parseJSON = Data.Aeson.withObject "LogBeginConvo" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ LogBeginConvo convoId rev

instance Data.Aeson.ToJSON LogBeginConvo where
  toJSON (LogBeginConvo convoId rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (LogBeginConvo convoId rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

logBeginConvo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogBeginConvo -> kv
logBeginConvo'AesonFields (LogBeginConvo convoId rev) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]

data LogCreateMessage = LogCreateMessage
  { convoId :: Data.Text.Text
  , message :: LogCreateMessageMessageKind
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogCreateMessage where
  parseJSON = Data.Aeson.withObject "LogCreateMessage" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    message <- v Data.Aeson..: Data.Aeson.Key.fromString "message"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ LogCreateMessage convoId message rev

instance Data.Aeson.ToJSON LogCreateMessage where
  toJSON (LogCreateMessage convoId message rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (LogCreateMessage convoId message rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

logCreateMessage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogCreateMessage -> kv
logCreateMessage'AesonFields (LogCreateMessage convoId message rev) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "message" Data.Aeson..= message
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]

data LogDeleteMessage = LogDeleteMessage
  { convoId :: Data.Text.Text
  , message :: LogDeleteMessageMessageKind
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogDeleteMessage where
  parseJSON = Data.Aeson.withObject "LogDeleteMessage" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    message <- v Data.Aeson..: Data.Aeson.Key.fromString "message"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ LogDeleteMessage convoId message rev

instance Data.Aeson.ToJSON LogDeleteMessage where
  toJSON (LogDeleteMessage convoId message rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (LogDeleteMessage convoId message rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

logDeleteMessage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogDeleteMessage -> kv
logDeleteMessage'AesonFields (LogDeleteMessage convoId message rev) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "message" Data.Aeson..= message
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]

data LogLeaveConvo = LogLeaveConvo
  { convoId :: Data.Text.Text
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogLeaveConvo where
  parseJSON = Data.Aeson.withObject "LogLeaveConvo" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ LogLeaveConvo convoId rev

instance Data.Aeson.ToJSON LogLeaveConvo where
  toJSON (LogLeaveConvo convoId rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (LogLeaveConvo convoId rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

logLeaveConvo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogLeaveConvo -> kv
logLeaveConvo'AesonFields (LogLeaveConvo convoId rev) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]

data MessageInput = MessageInput
  { embed :: Maybe MessageInputEmbedKind
  , facets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , text :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageInput where
  parseJSON = Data.Aeson.withObject "MessageInput" $ \v -> do
    embed <- v Data.Aeson..:? Data.Aeson.Key.fromString "embed"
    facets <- v Data.Aeson..:? Data.Aeson.Key.fromString "facets"
    text <- v Data.Aeson..: Data.Aeson.Key.fromString "text"
    pure $ MessageInput embed facets text

instance Data.Aeson.ToJSON MessageInput where
  toJSON (MessageInput embed facets text) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]
  toEncoding (MessageInput embed facets text) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]

messageInput'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageInput -> kv
messageInput'AesonFields (MessageInput embed facets text) =
  mconcat
    [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
    , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
    , Data.Aeson.Key.fromString "text" Data.Aeson..= text
    ]

data MessageRef = MessageRef
  { convoId :: Data.Text.Text
  , did :: Data.Text.Text
  , messageId :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageRef where
  parseJSON = Data.Aeson.withObject "MessageRef" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    messageId <- v Data.Aeson..: Data.Aeson.Key.fromString "messageId"
    pure $ MessageRef convoId did messageId

instance Data.Aeson.ToJSON MessageRef where
  toJSON (MessageRef convoId did messageId) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "messageId" Data.Aeson..= messageId
        ]
  toEncoding (MessageRef convoId did messageId) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "messageId" Data.Aeson..= messageId
        ]

messageRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageRef -> kv
messageRef'AesonFields (MessageRef convoId did messageId) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "messageId" Data.Aeson..= messageId
    ]

data MessageView = MessageView
  { embed :: Maybe MessageViewEmbedKind
  , facets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , id' :: Data.Text.Text
  , rev :: Data.Text.Text
  , sender :: MessageViewSender
  , sentAt :: Data.Text.Text
  , text :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageView where
  parseJSON = Data.Aeson.withObject "MessageView" $ \v -> do
    embed <- v Data.Aeson..:? Data.Aeson.Key.fromString "embed"
    facets <- v Data.Aeson..:? Data.Aeson.Key.fromString "facets"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    sender <- v Data.Aeson..: Data.Aeson.Key.fromString "sender"
    sentAt <- v Data.Aeson..: Data.Aeson.Key.fromString "sentAt"
    text <- v Data.Aeson..: Data.Aeson.Key.fromString "text"
    pure $ MessageView embed facets id' rev sender sentAt text

instance Data.Aeson.ToJSON MessageView where
  toJSON (MessageView embed facets id' rev sender sentAt text) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
        , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]
  toEncoding (MessageView embed facets id' rev sender sentAt text) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
        , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
        , Data.Aeson.Key.fromString "text" Data.Aeson..= text
        ]

messageView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageView -> kv
messageView'AesonFields (MessageView embed facets id' rev sender sentAt text) =
  mconcat
    [ Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
    , Data.Aeson.Key.fromString "facets" Data.Aeson..?= facets
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    , Data.Aeson.Key.fromString "sender" Data.Aeson..= sender
    , Data.Aeson.Key.fromString "sentAt" Data.Aeson..= sentAt
    , Data.Aeson.Key.fromString "text" Data.Aeson..= text
    ]

data MessageViewSender = MessageViewSender
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageViewSender where
  parseJSON = Data.Aeson.withObject "MessageViewSender" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ MessageViewSender did

instance Data.Aeson.ToJSON MessageViewSender where
  toJSON (MessageViewSender did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (MessageViewSender did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

messageViewSender'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageViewSender -> kv
messageViewSender'AesonFields (MessageViewSender did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

data ConvoViewLastMessageKind
  = ConvoViewLastMessageKindMessageView MessageView
  | ConvoViewLastMessageKindDeletedMessageView DeletedMessageView
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ConvoViewLastMessageKind where
  parseJSON = Data.Aeson.withObject "ConvoViewLastMessageKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#messageView" -> ConvoViewLastMessageKindMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#deletedMessageView" -> ConvoViewLastMessageKindDeletedMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ConvoViewLastMessageKind where
  toJSON = \case
    ConvoViewLastMessageKindMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    ConvoViewLastMessageKindDeletedMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
  toEncoding = \case
    ConvoViewLastMessageKindMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    ConvoViewLastMessageKindDeletedMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))

data LogCreateMessageMessageKind
  = LogCreateMessageMessageKindMessageView MessageView
  | LogCreateMessageMessageKindDeletedMessageView DeletedMessageView
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogCreateMessageMessageKind where
  parseJSON = Data.Aeson.withObject "LogCreateMessageMessageKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#messageView" -> LogCreateMessageMessageKindMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#deletedMessageView" -> LogCreateMessageMessageKindDeletedMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON LogCreateMessageMessageKind where
  toJSON = \case
    LogCreateMessageMessageKindMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    LogCreateMessageMessageKindDeletedMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
  toEncoding = \case
    LogCreateMessageMessageKindMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    LogCreateMessageMessageKindDeletedMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))

data LogDeleteMessageMessageKind
  = LogDeleteMessageMessageKindMessageView MessageView
  | LogDeleteMessageMessageKindDeletedMessageView DeletedMessageView
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LogDeleteMessageMessageKind where
  parseJSON = Data.Aeson.withObject "LogDeleteMessageMessageKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#messageView" -> LogDeleteMessageMessageKindMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#deletedMessageView" -> LogDeleteMessageMessageKindDeletedMessageView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON LogDeleteMessageMessageKind where
  toJSON = \case
    LogDeleteMessageMessageKindMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    LogDeleteMessageMessageKindDeletedMessageView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))
  toEncoding = \case
    LogDeleteMessageMessageKindMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageView") <> (Chat.Bsky.Convo.Defs.messageView'AesonFields v))
    LogDeleteMessageMessageKindDeletedMessageView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#deletedMessageView") <> (Chat.Bsky.Convo.Defs.deletedMessageView'AesonFields v))

data MessageInputEmbedKind
  = MessageInputEmbedKindRecord App.Bsky.Embed.Record.Record
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageInputEmbedKind where
  parseJSON = Data.Aeson.withObject "MessageInputEmbedKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.record#record" -> MessageInputEmbedKindRecord <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON MessageInputEmbedKind where
  toJSON = \case
    MessageInputEmbedKindRecord v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#record") <> (App.Bsky.Embed.Record.record'AesonFields v))
  toEncoding = \case
    MessageInputEmbedKindRecord v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#record") <> (App.Bsky.Embed.Record.record'AesonFields v))

data MessageViewEmbedKind
  = MessageViewEmbedKindView App.Bsky.Embed.Record.View
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MessageViewEmbedKind where
  parseJSON = Data.Aeson.withObject "MessageViewEmbedKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.record#view" -> MessageViewEmbedKindView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON MessageViewEmbedKind where
  toJSON = \case
    MessageViewEmbedKindView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
  toEncoding = \case
    MessageViewEmbedKindView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
