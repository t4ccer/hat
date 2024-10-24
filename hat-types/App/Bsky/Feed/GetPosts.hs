{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetPosts where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetPostsResult = GetPostsResult
  { posts :: [App.Bsky.Feed.Defs.PostView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetPostsResult where
  parseJSON = Data.Aeson.withObject "GetPostsResult" $ \v -> do
    posts <- v Data.Aeson..: Data.Aeson.Key.fromString "posts"
    pure $ GetPostsResult posts

instance Data.Aeson.ToJSON GetPostsResult where
  toJSON (GetPostsResult posts) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]
  toEncoding (GetPostsResult posts) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        ]

getPostsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPostsResult -> kv
getPostsResult'AesonFields (GetPostsResult posts) =
  mconcat
    [ Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
    ]

type GetPosts = "app.bsky.feed.getPosts" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uris" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetPostsResult
