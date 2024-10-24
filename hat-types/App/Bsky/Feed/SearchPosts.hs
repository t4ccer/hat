{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.SearchPosts where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SearchPostsResult = SearchPostsResult
  { cursor :: Maybe Data.Text.Text
  , hitsTotal :: Maybe Integer
  , posts :: [App.Bsky.Feed.Defs.PostView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchPostsResult where
  parseJSON = Data.Aeson.withObject "SearchPostsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    hitsTotal <- v Data.Aeson..:? Data.Aeson.Key.fromString "hitsTotal"
    posts <- v Data.Aeson..: Data.Aeson.Key.fromString "posts"
    pure $ SearchPostsResult cursor hitsTotal posts

instance Data.Aeson.ToJSON SearchPostsResult where
  toJSON (SearchPostsResult cursor hitsTotal posts) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]
  toEncoding (SearchPostsResult cursor hitsTotal posts) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]

searchPostsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchPostsResult -> kv
searchPostsResult'AesonFields (SearchPostsResult cursor hitsTotal posts) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "hitsTotal" Data.Aeson..?= hitsTotal
    , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
    ]

type SearchPosts = "app.bsky.feed.searchPosts" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "author" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "domain" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "lang" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "mentions" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "since" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sort" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "tag" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "until" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "url" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchPostsResult
