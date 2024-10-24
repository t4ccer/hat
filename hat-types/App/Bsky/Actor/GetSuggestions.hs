{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.GetSuggestions where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetSuggestionsResult = GetSuggestionsResult
  { actors :: [App.Bsky.Actor.Defs.ProfileView]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSuggestionsResult where
  parseJSON = Data.Aeson.withObject "GetSuggestionsResult" $ \v -> do
    actors <- v Data.Aeson..: Data.Aeson.Key.fromString "actors"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ GetSuggestionsResult actors cursor

instance Data.Aeson.ToJSON GetSuggestionsResult where
  toJSON (GetSuggestionsResult actors cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (GetSuggestionsResult actors cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

getSuggestionsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestionsResult -> kv
getSuggestionsResult'AesonFields (GetSuggestionsResult actors cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type GetSuggestions = "app.bsky.actor.getSuggestions" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetSuggestionsResult
