{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.Starterpack where

import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data FeedItem = FeedItem
  { uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FeedItem where
  parseJSON = Data.Aeson.withObject "FeedItem" $ \v -> do
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ FeedItem uri

instance Data.Aeson.ToJSON FeedItem where
  toJSON (FeedItem uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (FeedItem uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

feedItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedItem -> kv
feedItem'AesonFields (FeedItem uri) =
  mconcat
    [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data Starterpack = Starterpack
  { createdAt :: Data.Text.Text
  , description :: Maybe Data.Text.Text
  , descriptionFacets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , feeds :: Maybe [FeedItem]
  , list :: Data.Text.Text
  , name :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Starterpack where
  parseJSON = Data.Aeson.withObject "Starterpack" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    descriptionFacets <- v Data.Aeson..:? Data.Aeson.Key.fromString "descriptionFacets"
    feeds <- v Data.Aeson..:? Data.Aeson.Key.fromString "feeds"
    list <- v Data.Aeson..: Data.Aeson.Key.fromString "list"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    pure $ Starterpack createdAt description descriptionFacets feeds list name

instance Data.Aeson.ToJSON Starterpack where
  toJSON (Starterpack createdAt description descriptionFacets feeds list name) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]
  toEncoding (Starterpack createdAt description descriptionFacets feeds list name) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]

starterpack'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Starterpack -> kv
starterpack'AesonFields (Starterpack createdAt description descriptionFacets feeds list name) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
    , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
    , Data.Aeson.Key.fromString "list" Data.Aeson..= list
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    ]
