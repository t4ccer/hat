{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetFeedGenerators where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetFeedGeneratorsResult = GetFeedGeneratorsResult
  { feeds :: [App.Bsky.Feed.Defs.GeneratorView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetFeedGeneratorsResult where
  parseJSON = Data.Aeson.withObject "GetFeedGeneratorsResult" $ \v -> do
    feeds <- v Data.Aeson..: Data.Aeson.Key.fromString "feeds"
    pure $ GetFeedGeneratorsResult feeds

instance Data.Aeson.ToJSON GetFeedGeneratorsResult where
  toJSON (GetFeedGeneratorsResult feeds) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]
  toEncoding (GetFeedGeneratorsResult feeds) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        ]

getFeedGeneratorsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedGeneratorsResult -> kv
getFeedGeneratorsResult'AesonFields (GetFeedGeneratorsResult feeds) =
  mconcat
    [ Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
    ]

type GetFeedGenerators = "app.bsky.feed.getFeedGenerators" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "feeds" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetFeedGeneratorsResult
