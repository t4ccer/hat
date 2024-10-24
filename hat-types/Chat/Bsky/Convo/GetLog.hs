{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.GetLog where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetLogResult = GetLogResult
  { cursor :: Maybe Data.Text.Text
  , logs :: [GetLogResultLogsKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetLogResult where
  parseJSON = Data.Aeson.withObject "GetLogResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    logs <- v Data.Aeson..: Data.Aeson.Key.fromString "logs"
    pure $ GetLogResult cursor logs

instance Data.Aeson.ToJSON GetLogResult where
  toJSON (GetLogResult cursor logs) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "logs" Data.Aeson..= logs
        ]
  toEncoding (GetLogResult cursor logs) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "logs" Data.Aeson..= logs
        ]

getLogResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLogResult -> kv
getLogResult'AesonFields (GetLogResult cursor logs) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "logs" Data.Aeson..= logs
    ]

type GetLog = "chat.bsky.convo.getLog" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetLogResult
data GetLogResultLogsKind
  = GetLogResultLogsKindLogBeginConvo Chat.Bsky.Convo.Defs.LogBeginConvo
  | GetLogResultLogsKindLogLeaveConvo Chat.Bsky.Convo.Defs.LogLeaveConvo
  | GetLogResultLogsKindLogCreateMessage Chat.Bsky.Convo.Defs.LogCreateMessage
  | GetLogResultLogsKindLogDeleteMessage Chat.Bsky.Convo.Defs.LogDeleteMessage
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetLogResultLogsKind where
  parseJSON = Data.Aeson.withObject "GetLogResultLogsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "chat.bsky.convo.defs#logBeginConvo" -> GetLogResultLogsKindLogBeginConvo <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#logLeaveConvo" -> GetLogResultLogsKindLogLeaveConvo <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#logCreateMessage" -> GetLogResultLogsKindLogCreateMessage <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#logDeleteMessage" -> GetLogResultLogsKindLogDeleteMessage <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetLogResultLogsKind where
  toJSON = \case
    GetLogResultLogsKindLogBeginConvo v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logBeginConvo") <> (Chat.Bsky.Convo.Defs.logBeginConvo'AesonFields v))
    GetLogResultLogsKindLogLeaveConvo v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logLeaveConvo") <> (Chat.Bsky.Convo.Defs.logLeaveConvo'AesonFields v))
    GetLogResultLogsKindLogCreateMessage v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logCreateMessage") <> (Chat.Bsky.Convo.Defs.logCreateMessage'AesonFields v))
    GetLogResultLogsKindLogDeleteMessage v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logDeleteMessage") <> (Chat.Bsky.Convo.Defs.logDeleteMessage'AesonFields v))
  toEncoding = \case
    GetLogResultLogsKindLogBeginConvo v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logBeginConvo") <> (Chat.Bsky.Convo.Defs.logBeginConvo'AesonFields v))
    GetLogResultLogsKindLogLeaveConvo v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logLeaveConvo") <> (Chat.Bsky.Convo.Defs.logLeaveConvo'AesonFields v))
    GetLogResultLogsKindLogCreateMessage v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logCreateMessage") <> (Chat.Bsky.Convo.Defs.logCreateMessage'AesonFields v))
    GetLogResultLogsKindLogDeleteMessage v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#logDeleteMessage") <> (Chat.Bsky.Convo.Defs.logDeleteMessage'AesonFields v))
