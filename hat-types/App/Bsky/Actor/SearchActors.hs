{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.SearchActors where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchActorsResult = SearchActorsResult
  { actors :: [App.Bsky.Actor.Defs.ProfileView]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchActorsResult where
  parseJSON = Data.Aeson.withObject "SearchActorsResult" $ \v -> do
    actors <- v Data.Aeson..: Data.Aeson.Key.fromString "actors"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ SearchActorsResult actors cursor

instance Data.Aeson.ToJSON SearchActorsResult where
  toJSON (SearchActorsResult actors cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (SearchActorsResult actors cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

searchActorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsResult -> kv
searchActorsResult'AesonFields (SearchActorsResult actors cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type SearchActors = "app.bsky.actor.searchActors" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "term" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchActorsResult
