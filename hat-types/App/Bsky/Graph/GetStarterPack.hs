{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetStarterPack where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetStarterPackResult = GetStarterPackResult
  { starterPack :: App.Bsky.Graph.Defs.StarterPackView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetStarterPackResult where
  parseJSON = Data.Aeson.withObject "GetStarterPackResult" $ \v -> do
    starterPack <- v Data.Aeson..: Data.Aeson.Key.fromString "starterPack"
    pure $ GetStarterPackResult starterPack

instance Data.Aeson.ToJSON GetStarterPackResult where
  toJSON (GetStarterPackResult starterPack) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "starterPack" Data.Aeson..= starterPack
        ]
  toEncoding (GetStarterPackResult starterPack) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "starterPack" Data.Aeson..= starterPack
        ]

getStarterPackResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetStarterPackResult -> kv
getStarterPackResult'AesonFields (GetStarterPackResult starterPack) =
  mconcat
    [ Data.Aeson.Key.fromString "starterPack" Data.Aeson..= starterPack
    ]

type GetStarterPack = "app.bsky.graph.getStarterPack" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "starterPack" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetStarterPackResult
