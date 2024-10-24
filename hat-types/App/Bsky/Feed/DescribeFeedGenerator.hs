{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.DescribeFeedGenerator where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Feed = Feed
  { uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Feed where
  parseJSON = Data.Aeson.withObject "Feed" $ \v -> do
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ Feed uri

instance Data.Aeson.ToJSON Feed where
  toJSON (Feed uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (Feed uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

feed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Feed -> kv
feed'AesonFields (Feed uri) =
  mconcat
    [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data Links = Links
  { privacyPolicy :: Maybe Data.Text.Text
  , termsOfService :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Links where
  parseJSON = Data.Aeson.withObject "Links" $ \v -> do
    privacyPolicy <- v Data.Aeson..:? Data.Aeson.Key.fromString "privacyPolicy"
    termsOfService <- v Data.Aeson..:? Data.Aeson.Key.fromString "termsOfService"
    pure $ Links privacyPolicy termsOfService

instance Data.Aeson.ToJSON Links where
  toJSON (Links privacyPolicy termsOfService) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
        , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
        ]
  toEncoding (Links privacyPolicy termsOfService) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
        , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
        ]

links'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Links -> kv
links'AesonFields (Links privacyPolicy termsOfService) =
  mconcat
    [ Data.Aeson.Key.fromString "privacyPolicy" Data.Aeson..?= privacyPolicy
    , Data.Aeson.Key.fromString "termsOfService" Data.Aeson..?= termsOfService
    ]

data DescribeFeedGeneratorResult = DescribeFeedGeneratorResult
  { did :: Data.Text.Text
  , feeds :: [Feed]
  , links :: Maybe Links
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DescribeFeedGeneratorResult where
  parseJSON = Data.Aeson.withObject "DescribeFeedGeneratorResult" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    feeds <- v Data.Aeson..: Data.Aeson.Key.fromString "feeds"
    links <- v Data.Aeson..:? Data.Aeson.Key.fromString "links"
    pure $ DescribeFeedGeneratorResult did feeds links

instance Data.Aeson.ToJSON DescribeFeedGeneratorResult where
  toJSON (DescribeFeedGeneratorResult did feeds links) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
        ]
  toEncoding (DescribeFeedGeneratorResult did feeds links) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
        , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
        ]

describeFeedGeneratorResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeFeedGeneratorResult -> kv
describeFeedGeneratorResult'AesonFields (DescribeFeedGeneratorResult did feeds links) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "feeds" Data.Aeson..= feeds
    , Data.Aeson.Key.fromString "links" Data.Aeson..?= links
    ]

type DescribeFeedGenerator = "app.bsky.feed.describeFeedGenerator" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] DescribeFeedGeneratorResult
