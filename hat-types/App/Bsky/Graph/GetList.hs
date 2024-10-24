{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetList where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetListResult = GetListResult
  { cursor :: Maybe Data.Text.Text
  , items :: [App.Bsky.Graph.Defs.ListItemView]
  , list :: App.Bsky.Graph.Defs.ListView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetListResult where
  parseJSON = Data.Aeson.withObject "GetListResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    items <- v Data.Aeson..: Data.Aeson.Key.fromString "items"
    list <- v Data.Aeson..: Data.Aeson.Key.fromString "list"
    pure $ GetListResult cursor items list

instance Data.Aeson.ToJSON GetListResult where
  toJSON (GetListResult cursor items list) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "items" Data.Aeson..= items
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        ]
  toEncoding (GetListResult cursor items list) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "items" Data.Aeson..= items
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        ]

getListResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListResult -> kv
getListResult'AesonFields (GetListResult cursor items list) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "items" Data.Aeson..= items
    , Data.Aeson.Key.fromString "list" Data.Aeson..= list
    ]

type GetList = "app.bsky.graph.getList" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "list" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetListResult
