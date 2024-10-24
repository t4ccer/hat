{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Label.SubscribeLabels where

import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Info = Info
  { message :: Maybe Data.Text.Text
  , name :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Info where
  parseJSON = Data.Aeson.withObject "Info" $ \v -> do
    message <- v Data.Aeson..:? Data.Aeson.Key.fromString "message"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    pure $ Info message name

instance Data.Aeson.ToJSON Info where
  toJSON (Info message name) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]
  toEncoding (Info message name) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]

info'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Info -> kv
info'AesonFields (Info message name) =
  mconcat
    [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    ]

data Labels = Labels
  { labels :: [Com.Atproto.Label.Defs.Label]
  , seq :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Labels where
  parseJSON = Data.Aeson.withObject "Labels" $ \v -> do
    labels <- v Data.Aeson..: Data.Aeson.Key.fromString "labels"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    pure $ Labels labels seq

instance Data.Aeson.ToJSON Labels where
  toJSON (Labels labels seq) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        ]
  toEncoding (Labels labels seq) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        ]

labels'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Labels -> kv
labels'AesonFields (Labels labels seq) =
  mconcat
    [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    ]
