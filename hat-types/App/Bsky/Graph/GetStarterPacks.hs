{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetStarterPacks where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetStarterPacksResult = GetStarterPacksResult
  { starterPacks :: [App.Bsky.Graph.Defs.StarterPackViewBasic]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetStarterPacksResult where
  parseJSON = Data.Aeson.withObject "GetStarterPacksResult" $ \v -> do
    starterPacks <- v Data.Aeson..: Data.Aeson.Key.fromString "starterPacks"
    pure $ GetStarterPacksResult starterPacks

instance Data.Aeson.ToJSON GetStarterPacksResult where
  toJSON (GetStarterPacksResult starterPacks) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
        ]
  toEncoding (GetStarterPacksResult starterPacks) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
        ]

getStarterPacksResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetStarterPacksResult -> kv
getStarterPacksResult'AesonFields (GetStarterPacksResult starterPacks) =
  mconcat
    [ Data.Aeson.Key.fromString "starterPacks" Data.Aeson..= starterPacks
    ]

type GetStarterPacks = "app.bsky.graph.getStarterPacks" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uris" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetStarterPacksResult
