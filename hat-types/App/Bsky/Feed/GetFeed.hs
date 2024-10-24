{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetFeed where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetFeedResult = GetFeedResult
  { cursor :: Maybe Data.Text.Text
  , feed :: [App.Bsky.Feed.Defs.FeedViewPost]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetFeedResult where
  parseJSON = Data.Aeson.withObject "GetFeedResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    feed <- v Data.Aeson..: Data.Aeson.Key.fromString "feed"
    pure $ GetFeedResult cursor feed

instance Data.Aeson.ToJSON GetFeedResult where
  toJSON (GetFeedResult cursor feed) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]
  toEncoding (GetFeedResult cursor feed) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]

getFeedResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedResult -> kv
getFeedResult'AesonFields (GetFeedResult cursor feed) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
    ]

type GetFeed = "app.bsky.feed.getFeed" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "feed" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetFeedResult
