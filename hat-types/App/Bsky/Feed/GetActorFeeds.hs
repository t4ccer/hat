{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetActorFeeds where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetActorFeedsResult = GetActorFeedsResult
  { cursor :: Maybe Data.Text.Text
  , feeds :: [App.Bsky.Feed.Defs.GeneratorView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetActorFeedsResult where
  parseJSON = Data.Aeson.withObject "GetActorFeedsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    feeds <- v Data.Aeson..: Data.Aeson.Key.fromString "feeds"
    pure $ GetActorFeedsResult cursor feeds

instance Data.Aeson.ToJSON GetActorFeedsResult where
  toJSON (GetActorFeedsResult cursor feeds) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]
  toEncoding (GetActorFeedsResult cursor feeds) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]

getActorFeedsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetActorFeedsResult -> kv
getActorFeedsResult'AesonFields (GetActorFeedsResult cursor feeds) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
    ]

type GetActorFeeds = "app.bsky.feed.getActorFeeds" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetActorFeedsResult
