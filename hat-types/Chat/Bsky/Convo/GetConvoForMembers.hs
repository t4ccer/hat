{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Convo.GetConvoForMembers where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetConvoForMembersResult = GetConvoForMembersResult
  { convo :: Chat.Bsky.Convo.Defs.ConvoView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetConvoForMembersResult where
  parseJSON = Data.Aeson.withObject "GetConvoForMembersResult" $ \v -> do
    convo <- v Data.Aeson..: Data.Aeson.Key.fromString "convo"
    pure $ GetConvoForMembersResult convo

instance Data.Aeson.ToJSON GetConvoForMembersResult where
  toJSON (GetConvoForMembersResult convo) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
        ]
  toEncoding (GetConvoForMembersResult convo) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
        ]

getConvoForMembersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConvoForMembersResult -> kv
getConvoForMembersResult'AesonFields (GetConvoForMembersResult convo) =
  mconcat
    [ Data.Aeson.Key.fromString "convo" Data.Aeson..= convo
    ]

type GetConvoForMembers = "chat.bsky.convo.getConvoForMembers" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "members" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetConvoForMembersResult
