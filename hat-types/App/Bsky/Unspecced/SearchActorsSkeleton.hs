{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.SearchActorsSkeleton where

import {-# SOURCE #-} qualified App.Bsky.Unspecced.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchActorsSkeletonResult = SearchActorsSkeletonResult
  { actors :: [App.Bsky.Unspecced.Defs.SkeletonSearchActor]
  , cursor :: Maybe Data.Text.Text
  , hitsTotal :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchActorsSkeletonResult where
  parseJSON = Data.Aeson.withObject "SearchActorsSkeletonResult" $ \v -> do
    actors <- v Data.Aeson..: Data.Aeson.Key.fromString "actors"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    hitsTotal <- v Data.Aeson..:? Data.Aeson.Key.fromString "hitsTotal"
    pure $ SearchActorsSkeletonResult actors cursor hitsTotal

instance Data.Aeson.ToJSON SearchActorsSkeletonResult where
  toJSON (SearchActorsSkeletonResult actors cursor hitsTotal) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        ]
  toEncoding (SearchActorsSkeletonResult actors cursor hitsTotal) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        ]

searchActorsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchActorsSkeletonResult -> kv
searchActorsSkeletonResult'AesonFields (SearchActorsSkeletonResult actors cursor hitsTotal) =
  mconcat
    [ Data.Aeson.Key.fromString "actors" Data.Aeson..= actors
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
    ]

type SearchActorsSkeleton = "app.bsky.unspecced.searchActorsSkeleton" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "typeahead" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "viewer" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchActorsSkeletonResult
