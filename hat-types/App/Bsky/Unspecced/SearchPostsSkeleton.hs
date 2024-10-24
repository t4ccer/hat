{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.SearchPostsSkeleton where

import {-# SOURCE #-} qualified App.Bsky.Unspecced.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchPostsSkeletonResult = SearchPostsSkeletonResult
  { cursor :: Maybe Data.Text.Text
  , hitsTotal :: Maybe Integer
  , posts :: [App.Bsky.Unspecced.Defs.SkeletonSearchPost]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchPostsSkeletonResult where
  parseJSON = Data.Aeson.withObject "SearchPostsSkeletonResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    hitsTotal <- v Data.Aeson..:? Data.Aeson.Key.fromString "hitsTotal"
    posts <- v Data.Aeson..: Data.Aeson.Key.fromString "posts"
    pure $ SearchPostsSkeletonResult cursor hitsTotal posts

instance Data.Aeson.ToJSON SearchPostsSkeletonResult where
  toJSON (SearchPostsSkeletonResult cursor hitsTotal posts) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]
  toEncoding (SearchPostsSkeletonResult cursor hitsTotal posts) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]

searchPostsSkeletonResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchPostsSkeletonResult -> kv
searchPostsSkeletonResult'AesonFields (SearchPostsSkeletonResult cursor hitsTotal posts) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
    , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
    ]

type SearchPostsSkeleton = "app.bsky.unspecced.searchPostsSkeleton" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "author" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "domain" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "lang" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "mentions" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "since" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sort" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "tag" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "until" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "url" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "viewer" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchPostsSkeletonResult
