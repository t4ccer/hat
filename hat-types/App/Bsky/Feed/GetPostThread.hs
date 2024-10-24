{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetPostThread where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetPostThreadResult = GetPostThreadResult
  { thread :: GetPostThreadResultThreadKind
  , threadgate :: Maybe App.Bsky.Feed.Defs.ThreadgateView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetPostThreadResult where
  parseJSON = Data.Aeson.withObject "GetPostThreadResult" $ \v -> do
    thread <- v Data.Aeson..: Data.Aeson.Key.fromString "thread"
    threadgate <- v Data.Aeson..:? Data.Aeson.Key.fromString "threadgate"
    pure $ GetPostThreadResult thread threadgate

instance Data.Aeson.ToJSON GetPostThreadResult where
  toJSON (GetPostThreadResult thread threadgate) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "thread" Data.Aeson..= thread
        , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
        ]
  toEncoding (GetPostThreadResult thread threadgate) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "thread" Data.Aeson..= thread
        , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
        ]

getPostThreadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPostThreadResult -> kv
getPostThreadResult'AesonFields (GetPostThreadResult thread threadgate) =
  mconcat
    [ Data.Aeson.Key.fromString "thread" Data.Aeson..= thread
    , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
    ]

type GetPostThread = "app.bsky.feed.getPostThread" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "depth" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "parentHeight" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uri" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetPostThreadResult
data GetPostThreadResultThreadKind
  = GetPostThreadResultThreadKindThreadViewPost App.Bsky.Feed.Defs.ThreadViewPost
  | GetPostThreadResultThreadKindNotFoundPost App.Bsky.Feed.Defs.NotFoundPost
  | GetPostThreadResultThreadKindBlockedPost App.Bsky.Feed.Defs.BlockedPost
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetPostThreadResultThreadKind where
  parseJSON = Data.Aeson.withObject "GetPostThreadResultThreadKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#threadViewPost" -> GetPostThreadResultThreadKindThreadViewPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#notFoundPost" -> GetPostThreadResultThreadKindNotFoundPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#blockedPost" -> GetPostThreadResultThreadKindBlockedPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetPostThreadResultThreadKind where
  toJSON = \case
    GetPostThreadResultThreadKindThreadViewPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    GetPostThreadResultThreadKindNotFoundPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    GetPostThreadResultThreadKindBlockedPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
  toEncoding = \case
    GetPostThreadResultThreadKindThreadViewPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    GetPostThreadResultThreadKindNotFoundPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    GetPostThreadResultThreadKindBlockedPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
