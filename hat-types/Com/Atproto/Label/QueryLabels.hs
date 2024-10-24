{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Label.QueryLabels where

import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data QueryLabelsResult = QueryLabelsResult
  { cursor :: Maybe Data.Text.Text
  , labels :: [Com.Atproto.Label.Defs.Label]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON QueryLabelsResult where
  parseJSON = Data.Aeson.withObject "QueryLabelsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    labels <- v Data.Aeson..: Data.Aeson.Key.fromString "labels"
    pure $ QueryLabelsResult cursor labels

instance Data.Aeson.ToJSON QueryLabelsResult where
  toJSON (QueryLabelsResult cursor labels) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        ]
  toEncoding (QueryLabelsResult cursor labels) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        ]

queryLabelsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryLabelsResult -> kv
queryLabelsResult'AesonFields (QueryLabelsResult cursor labels) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
    ]

type QueryLabels = "com.atproto.label.queryLabels" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sources" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uriPatterns" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] QueryLabelsResult
