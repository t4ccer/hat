{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.SearchActorsTypeahead where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchActorsTypeaheadResult = SearchActorsTypeaheadResult
  { actors :: [App.Bsky.Actor.Defs.ProfileViewBasic]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchActorsTypeaheadResult where
  parseJSON = Data.Aeson.withObject "SearchActorsTypeaheadResult" $ \v -> do
    actors <- v Data.Aeson..: Data.Aeson.Key.fromString "actors"
    pure $ SearchActorsTypeaheadResult actors

instance Data.Aeson.ToJSON SearchActorsTypeaheadResult where
  toJSON (SearchActorsTypeaheadResult actors) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        ]
  toEncoding (SearchActorsTypeaheadResult actors) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        ]

searchActorsTypeaheadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsTypeaheadResult -> kv
searchActorsTypeaheadResult'AesonFields (SearchActorsTypeaheadResult actors) =
  mconcat
    [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
    ]

type SearchActorsTypeahead = "app.bsky.actor.searchActorsTypeahead" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "term" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchActorsTypeaheadResult
