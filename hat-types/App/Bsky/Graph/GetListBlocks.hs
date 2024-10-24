{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetListBlocks where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetListBlocksResult = GetListBlocksResult
  { cursor :: Maybe Data.Text.Text
  , lists :: [App.Bsky.Graph.Defs.ListView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetListBlocksResult where
  parseJSON = Data.Aeson.withObject "GetListBlocksResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    lists <- v Data.Aeson..: Data.Aeson.Key.fromString "lists"
    pure $ GetListBlocksResult cursor lists

instance Data.Aeson.ToJSON GetListBlocksResult where
  toJSON (GetListBlocksResult cursor lists) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]
  toEncoding (GetListBlocksResult cursor lists) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]

getListBlocksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListBlocksResult -> kv
getListBlocksResult'AesonFields (GetListBlocksResult cursor lists) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
    ]

type GetListBlocks = "app.bsky.graph.getListBlocks" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetListBlocksResult
