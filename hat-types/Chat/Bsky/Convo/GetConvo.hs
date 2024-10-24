{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.GetConvo where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetConvoResult = GetConvoResult
  { convo :: Chat.Bsky.Convo.Defs.ConvoView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetConvoResult where
  parseJSON = Data.Aeson.withObject "GetConvoResult" $ \v -> do
    convo <- v Data.Aeson..: Data.Aeson.Key.fromString "convo"
    pure $ GetConvoResult convo

instance Data.Aeson.ToJSON GetConvoResult where
  toJSON (GetConvoResult convo) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
        ]
  toEncoding (GetConvoResult convo) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
        ]

getConvoResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConvoResult -> kv
getConvoResult'AesonFields (GetConvoResult convo) =
  mconcat
    [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
    ]

type GetConvo = "chat.bsky.convo.getConvo" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "convoId" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetConvoResult
