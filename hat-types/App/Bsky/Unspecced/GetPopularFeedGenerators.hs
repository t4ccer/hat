{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.GetPopularFeedGenerators where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetPopularFeedGeneratorsResult = GetPopularFeedGeneratorsResult
  { cursor :: Maybe Data.Text.Text
  , feeds :: [App.Bsky.Feed.Defs.GeneratorView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetPopularFeedGeneratorsResult where
  parseJSON = Data.Aeson.withObject "GetPopularFeedGeneratorsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    feeds <- v Data.Aeson..: Data.Aeson.Key.fromString "feeds"
    pure $ GetPopularFeedGeneratorsResult cursor feeds

instance Data.Aeson.ToJSON GetPopularFeedGeneratorsResult where
  toJSON (GetPopularFeedGeneratorsResult cursor feeds) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]
  toEncoding (GetPopularFeedGeneratorsResult cursor feeds) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]

getPopularFeedGeneratorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPopularFeedGeneratorsResult -> kv
getPopularFeedGeneratorsResult'AesonFields (GetPopularFeedGeneratorsResult cursor feeds) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
    ]

type GetPopularFeedGenerators = "app.bsky.unspecced.getPopularFeedGenerators" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "query" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetPopularFeedGeneratorsResult
