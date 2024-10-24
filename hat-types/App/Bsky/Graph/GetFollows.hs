{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetFollows where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetFollowsResult = GetFollowsResult
  { cursor :: Maybe Data.Text.Text
  , follows :: [App.Bsky.Actor.Defs.ProfileView]
  , subject :: App.Bsky.Actor.Defs.ProfileView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetFollowsResult where
  parseJSON = Data.Aeson.withObject "GetFollowsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    follows <- v Data.Aeson..: Data.Aeson.Key.fromString "follows"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ GetFollowsResult cursor follows subject

instance Data.Aeson.ToJSON GetFollowsResult where
  toJSON (GetFollowsResult cursor follows subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "follows" Data.Aeson..= follows
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (GetFollowsResult cursor follows subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "follows" Data.Aeson..= follows
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

getFollowsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFollowsResult -> kv
getFollowsResult'AesonFields (GetFollowsResult cursor follows subject) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "follows" Data.Aeson..= follows
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]

type GetFollows = "app.bsky.graph.getFollows" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetFollowsResult
