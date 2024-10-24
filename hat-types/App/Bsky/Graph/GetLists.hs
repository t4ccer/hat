{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetLists where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetListsResult = GetListsResult
  { cursor :: Maybe Data.Text.Text
  , lists :: [App.Bsky.Graph.Defs.ListView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetListsResult where
  parseJSON = Data.Aeson.withObject "GetListsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    lists <- v Data.Aeson..: Data.Aeson.Key.fromString "lists"
    pure $ GetListsResult cursor lists

instance Data.Aeson.ToJSON GetListsResult where
  toJSON (GetListsResult cursor lists) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]
  toEncoding (GetListsResult cursor lists) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]

getListsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListsResult -> kv
getListsResult'AesonFields (GetListsResult cursor lists) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
    ]

type GetLists = "app.bsky.graph.getLists" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetListsResult
