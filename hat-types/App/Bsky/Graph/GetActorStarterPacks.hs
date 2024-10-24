{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetActorStarterPacks where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetActorStarterPacksResult = GetActorStarterPacksResult
  { cursor :: Maybe Data.Text.Text
  , starterPacks :: [App.Bsky.Graph.Defs.StarterPackViewBasic]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetActorStarterPacksResult where
  parseJSON = Data.Aeson.withObject "GetActorStarterPacksResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    starterPacks <- v Data.Aeson..: Data.Aeson.Key.fromString "starterPacks"
    pure $ GetActorStarterPacksResult cursor starterPacks

instance Data.Aeson.ToJSON GetActorStarterPacksResult where
  toJSON (GetActorStarterPacksResult cursor starterPacks) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
        ]
  toEncoding (GetActorStarterPacksResult cursor starterPacks) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
        ]

getActorStarterPacksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorStarterPacksResult -> kv
getActorStarterPacksResult'AesonFields (GetActorStarterPacksResult cursor starterPacks) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
    ]

type GetActorStarterPacks = "app.bsky.graph.getActorStarterPacks" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetActorStarterPacksResult
