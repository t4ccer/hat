{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetLikes where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Like = Like
  { actor :: App.Bsky.Actor.Defs.ProfileView
  , createdAt :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Like where
  parseJSON = Data.Aeson.withObject "Like" $ \v -> do
    actor <- v Data.Aeson..: Data.Aeson.Key.fromString "actor"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    pure $ Like actor createdAt indexedAt

instance Data.Aeson.ToJSON Like where
  toJSON (Like actor createdAt indexedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        ]
  toEncoding (Like actor createdAt indexedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        ]

like'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Like -> kv
like'AesonFields (Like actor createdAt indexedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    ]

data GetLikesResult = GetLikesResult
  { cid :: Maybe Data.Text.Text
  , cursor :: Maybe Data.Text.Text
  , likes :: [Like]
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetLikesResult where
  parseJSON = Data.Aeson.withObject "GetLikesResult" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    likes <- v Data.Aeson..: Data.Aeson.Key.fromString "likes"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ GetLikesResult cid cursor likes uri

instance Data.Aeson.ToJSON GetLikesResult where
  toJSON (GetLikesResult cid cursor likes uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "likes" Data.Aeson..= likes
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (GetLikesResult cid cursor likes uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "likes" Data.Aeson..= likes
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

getLikesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLikesResult -> kv
getLikesResult'AesonFields (GetLikesResult cid cursor likes uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "likes" Data.Aeson..= likes
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

type GetLikes = "app.bsky.feed.getLikes" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cid" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uri" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetLikesResult
