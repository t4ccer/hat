{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetBlocks where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetBlocksResult = GetBlocksResult
  { blocks :: [App.Bsky.Actor.Defs.ProfileView]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetBlocksResult where
  parseJSON = Data.Aeson.withObject "GetBlocksResult" $ \v -> do
    blocks <- v Data.Aeson..: Data.Aeson.Key.fromString "blocks"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ GetBlocksResult blocks cursor

instance Data.Aeson.ToJSON GetBlocksResult where
  toJSON (GetBlocksResult blocks cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blocks" Data.Aeson..= blocks
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (GetBlocksResult blocks cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blocks" Data.Aeson..= blocks
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

getBlocksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetBlocksResult -> kv
getBlocksResult'AesonFields (GetBlocksResult blocks cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "blocks" Data.Aeson..= blocks
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type GetBlocks = "app.bsky.graph.getBlocks" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetBlocksResult
