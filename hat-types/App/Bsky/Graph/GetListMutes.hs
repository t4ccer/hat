{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetListMutes where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetListMutesResult = GetListMutesResult
  { cursor :: Maybe Data.Text.Text
  , lists :: [App.Bsky.Graph.Defs.ListView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetListMutesResult where
  parseJSON = Data.Aeson.withObject "GetListMutesResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    lists <- v Data.Aeson..: Data.Aeson.Key.fromString "lists"
    pure $ GetListMutesResult cursor lists

instance Data.Aeson.ToJSON GetListMutesResult where
  toJSON (GetListMutesResult cursor lists) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]
  toEncoding (GetListMutesResult cursor lists) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
        ]

getListMutesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetListMutesResult -> kv
getListMutesResult'AesonFields (GetListMutesResult cursor lists) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "lists" Data.Aeson..= lists
    ]

type GetListMutes = "app.bsky.graph.getListMutes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetListMutesResult
