{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.GetFeedGenerator where

import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetFeedGeneratorResult = GetFeedGeneratorResult
  { isOnline :: Bool
  , isValid :: Bool
  , view :: App.Bsky.Feed.Defs.GeneratorView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetFeedGeneratorResult where
  parseJSON = Data.Aeson.withObject "GetFeedGeneratorResult" $ \v -> do
    isOnline <- v Data.Aeson..: Data.Aeson.Key.fromString "isOnline"
    isValid <- v Data.Aeson..: Data.Aeson.Key.fromString "isValid"
    view <- v Data.Aeson..: Data.Aeson.Key.fromString "view"
    pure $ GetFeedGeneratorResult isOnline isValid view

instance Data.Aeson.ToJSON GetFeedGeneratorResult where
  toJSON (GetFeedGeneratorResult isOnline isValid view) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "isOnline" Data.Aeson..= isOnline
        , Data.Aeson.Key.fromString "isValid" Data.Aeson..= isValid
        , Data.Aeson.Key.fromString "view" Data.Aeson..= view
        ]
  toEncoding (GetFeedGeneratorResult isOnline isValid view) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "isOnline" Data.Aeson..= isOnline
        , Data.Aeson.Key.fromString "isValid" Data.Aeson..= isValid
        , Data.Aeson.Key.fromString "view" Data.Aeson..= view
        ]

getFeedGeneratorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetFeedGeneratorResult -> kv
getFeedGeneratorResult'AesonFields (GetFeedGeneratorResult isOnline isValid view) =
  mconcat
    [ Data.Aeson.Key.fromString "isOnline" Data.Aeson..= isOnline
    , Data.Aeson.Key.fromString "isValid" Data.Aeson..= isValid
    , Data.Aeson.Key.fromString "view" Data.Aeson..= view
    ]

type GetFeedGenerator = "app.bsky.feed.getFeedGenerator" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "feed" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetFeedGeneratorResult
