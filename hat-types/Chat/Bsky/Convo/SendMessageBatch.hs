{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.SendMessageBatch where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data BatchItem = BatchItem
  { convoId :: Data.Text.Text
  , message :: Chat.Bsky.Convo.Defs.MessageInput
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BatchItem where
  parseJSON = Data.Aeson.withObject "BatchItem" $ \v -> do
    convoId <- v Data.Aeson..: Data.Aeson.Key.fromString "convoId"
    message <- v Data.Aeson..: Data.Aeson.Key.fromString "message"
    pure $ BatchItem convoId message

instance Data.Aeson.ToJSON BatchItem where
  toJSON (BatchItem convoId message) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        ]
  toEncoding (BatchItem convoId message) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
        , Data.Aeson.Key.fromString "message" Data.Aeson..= message
        ]

batchItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BatchItem -> kv
batchItem'AesonFields (BatchItem convoId message) =
  mconcat
    [ Data.Aeson.Key.fromString "convoId" Data.Aeson..= convoId
    , Data.Aeson.Key.fromString "message" Data.Aeson..= message
    ]
