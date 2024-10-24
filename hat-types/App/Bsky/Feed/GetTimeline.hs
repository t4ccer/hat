{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetTimeline where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetTimelineResult = GetTimelineResult
  { cursor :: Maybe Data.Text.Text
  , feed :: [App.Bsky.Feed.Defs.FeedViewPost]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetTimelineResult where
  parseJSON = Data.Aeson.withObject "GetTimelineResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    feed <- v Data.Aeson..: Data.Aeson.Key.fromString "feed"
    pure $ GetTimelineResult cursor feed

instance Data.Aeson.ToJSON GetTimelineResult where
  toJSON (GetTimelineResult cursor feed) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]
  toEncoding (GetTimelineResult cursor feed) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        ]

getTimelineResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetTimelineResult -> kv
getTimelineResult'AesonFields (GetTimelineResult cursor feed) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
    ]

type GetTimeline = "app.bsky.feed.getTimeline" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "algorithm" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetTimelineResult
