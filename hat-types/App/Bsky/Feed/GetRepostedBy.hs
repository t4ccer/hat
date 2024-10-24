{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetRepostedBy where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetRepostedByResult = GetRepostedByResult
  { cid :: Maybe Data.Text.Text
  , cursor :: Maybe Data.Text.Text
  , repostedBy :: [App.Bsky.Actor.Defs.ProfileView]
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRepostedByResult where
  parseJSON = Data.Aeson.withObject "GetRepostedByResult" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    repostedBy <- v Data.Aeson..: Data.Aeson.Key.fromString "repostedBy"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ GetRepostedByResult cid cursor repostedBy uri

instance Data.Aeson.ToJSON GetRepostedByResult where
  toJSON (GetRepostedByResult cid cursor repostedBy uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repostedBy" Data.Aeson..= repostedBy
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (GetRepostedByResult cid cursor repostedBy uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repostedBy" Data.Aeson..= repostedBy
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

getRepostedByResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRepostedByResult -> kv
getRepostedByResult'AesonFields (GetRepostedByResult cid cursor repostedBy uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "repostedBy" Data.Aeson..= repostedBy
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

type GetRepostedBy = "app.bsky.feed.getRepostedBy" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cid" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uri" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRepostedByResult
