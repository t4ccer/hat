{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetQuotes where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetQuotesResult = GetQuotesResult
  { cid :: Maybe Data.Text.Text
  , cursor :: Maybe Data.Text.Text
  , posts :: [App.Bsky.Feed.Defs.PostView]
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetQuotesResult where
  parseJSON = Data.Aeson.withObject "GetQuotesResult" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    posts <- v Data.Aeson..: Data.Aeson.Key.fromString "posts"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ GetQuotesResult cid cursor posts uri

instance Data.Aeson.ToJSON GetQuotesResult where
  toJSON (GetQuotesResult cid cursor posts uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (GetQuotesResult cid cursor posts uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

getQuotesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetQuotesResult -> kv
getQuotesResult'AesonFields (GetQuotesResult cid cursor posts uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "posts" Data.Aeson..= posts
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

type GetQuotes = "app.bsky.feed.getQuotes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cid" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uri" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetQuotesResult
