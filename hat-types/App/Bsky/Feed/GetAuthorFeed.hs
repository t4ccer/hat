{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetAuthorFeed where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetAuthorFeedResult = GetAuthorFeedResult
  { cursor :: Maybe Data.Text.Text
  , feed :: [App.Bsky.Feed.Defs.FeedViewPost]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetAuthorFeedResult where
  parseJSON = Data.Aeson.withObject "GetAuthorFeedResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    feed <- v Data.Aeson..: Data.Aeson.Key.fromString "feed"
    pure $ GetAuthorFeedResult cursor feed

instance Data.Aeson.ToJSON GetAuthorFeedResult where
  toJSON (GetAuthorFeedResult cursor feed) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]
  toEncoding (GetAuthorFeedResult cursor feed) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]

getAuthorFeedResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetAuthorFeedResult -> kv
getAuthorFeedResult'AesonFields (GetAuthorFeedResult cursor feed) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
    ]

type GetAuthorFeed = "app.bsky.feed.getAuthorFeed" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "filter" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "includePins" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetAuthorFeedResult
