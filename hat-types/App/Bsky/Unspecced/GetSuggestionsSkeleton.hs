{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.GetSuggestionsSkeleton where

import {-# SOURCE #-} qualified App.Bsky.Unspecced.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetSuggestionsSkeletonResult = GetSuggestionsSkeletonResult
  { actors :: [App.Bsky.Unspecced.Defs.SkeletonSearchActor]
  , cursor :: Maybe Data.Text.Text
  , relativeToDid :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSuggestionsSkeletonResult where
  parseJSON = Data.Aeson.withObject "GetSuggestionsSkeletonResult" $ \v -> do
    actors <- v Data.Aeson..: Data.Aeson.Key.fromString "actors"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    relativeToDid <- v Data.Aeson..:? Data.Aeson.Key.fromString "relativeToDid"
    pure $ GetSuggestionsSkeletonResult actors cursor relativeToDid

instance Data.Aeson.ToJSON GetSuggestionsSkeletonResult where
  toJSON (GetSuggestionsSkeletonResult actors cursor relativeToDid) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "relativeToDid" Data.Aeson..?= relativeToDid
        ]
  toEncoding (GetSuggestionsSkeletonResult actors cursor relativeToDid) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "relativeToDid" Data.Aeson..?= relativeToDid
        ]

getSuggestionsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSuggestionsSkeletonResult -> kv
getSuggestionsSkeletonResult'AesonFields (GetSuggestionsSkeletonResult actors cursor relativeToDid) =
  mconcat
    [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "relativeToDid" Data.Aeson..?= relativeToDid
    ]

type GetSuggestionsSkeleton = "app.bsky.unspecced.getSuggestionsSkeleton" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "relativeToDid" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "viewer" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetSuggestionsSkeletonResult
