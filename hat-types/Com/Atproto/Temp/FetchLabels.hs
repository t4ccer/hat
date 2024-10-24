{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Temp.FetchLabels where

import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data FetchLabelsResult = FetchLabelsResult
  { labels :: [Com.Atproto.Label.Defs.Label]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FetchLabelsResult where
  parseJSON = Data.Aeson.withObject "FetchLabelsResult" $ \v -> do
    labels <- v Data.Aeson..: Data.Aeson.Key.fromString "labels"
    pure $ FetchLabelsResult labels

instance Data.Aeson.ToJSON FetchLabelsResult where
  toJSON (FetchLabelsResult labels) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        ]
  toEncoding (FetchLabelsResult labels) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
        ]

fetchLabelsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FetchLabelsResult -> kv
fetchLabelsResult'AesonFields (FetchLabelsResult labels) =
  mconcat
    [ Data.Aeson.Key.fromString "labels" Data.Aeson..= labels
    ]

type FetchLabels = "com.atproto.temp.fetchLabels" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "since" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] FetchLabelsResult
