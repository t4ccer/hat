{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.GetFollowers where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetFollowersResult = GetFollowersResult
  { cursor :: Maybe Data.Text.Text
  , followers :: [App.Bsky.Actor.Defs.ProfileView]
  , subject :: App.Bsky.Actor.Defs.ProfileView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetFollowersResult where
  parseJSON = Data.Aeson.withObject "GetFollowersResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    followers <- v Data.Aeson..: Data.Aeson.Key.fromString "followers"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ GetFollowersResult cursor followers subject

instance Data.Aeson.ToJSON GetFollowersResult where
  toJSON (GetFollowersResult cursor followers subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (GetFollowersResult cursor followers subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

getFollowersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFollowersResult -> kv
getFollowersResult'AesonFields (GetFollowersResult cursor followers subject) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]

type GetFollowers = "app.bsky.graph.getFollowers" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "actor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetFollowersResult
