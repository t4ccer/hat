{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.ListConvos where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListConvosResult = ListConvosResult
  { convos :: [Chat.Bsky.Convo.Defs.ConvoView]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListConvosResult where
  parseJSON = Data.Aeson.withObject "ListConvosResult" $ \v -> do
    convos <- v Data.Aeson..: Data.Aeson.Key.fromString "convos"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ ListConvosResult convos cursor

instance Data.Aeson.ToJSON ListConvosResult where
  toJSON (ListConvosResult convos cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (ListConvosResult convos cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

listConvosResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListConvosResult -> kv
listConvosResult'AesonFields (ListConvosResult convos cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "convos" Data.Aeson..= convos
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type ListConvos = "chat.bsky.convo.listConvos" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListConvosResult
