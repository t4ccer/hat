{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetMutes where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetMutesResult = GetMutesResult
  { cursor :: Maybe Data.Text.Text
  , mutes :: [App.Bsky.Actor.Defs.ProfileView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetMutesResult where
  parseJSON = Data.Aeson.withObject "GetMutesResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    mutes <- v Data.Aeson..: Data.Aeson.Key.fromString "mutes"
    pure $ GetMutesResult cursor mutes

instance Data.Aeson.ToJSON GetMutesResult where
  toJSON (GetMutesResult cursor mutes) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "mutes" Data.Aeson..= mutes
        ]
  toEncoding (GetMutesResult cursor mutes) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "mutes" Data.Aeson..= mutes
        ]

getMutesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetMutesResult -> kv
getMutesResult'AesonFields (GetMutesResult cursor mutes) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "mutes" Data.Aeson..= mutes
    ]

type GetMutes = "app.bsky.graph.getMutes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetMutesResult
