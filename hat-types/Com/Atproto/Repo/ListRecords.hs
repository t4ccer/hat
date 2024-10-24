{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.ListRecords where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListRecordsResult = ListRecordsResult
  { cursor :: Maybe Data.Text.Text
  , records :: [Record]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListRecordsResult where
  parseJSON = Data.Aeson.withObject "ListRecordsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    records <- v Data.Aeson..: Data.Aeson.Key.fromString "records"
    pure $ ListRecordsResult cursor records

instance Data.Aeson.ToJSON ListRecordsResult where
  toJSON (ListRecordsResult cursor records) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "records" Data.Aeson..= records
        ]
  toEncoding (ListRecordsResult cursor records) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "records" Data.Aeson..= records
        ]

listRecordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListRecordsResult -> kv
listRecordsResult'AesonFields (ListRecordsResult cursor records) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "records" Data.Aeson..= records
    ]

type ListRecords = "com.atproto.repo.listRecords" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "collection" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "repo" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reverse" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "rkeyEnd" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "rkeyStart" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListRecordsResult
data Record = Record
  { cid :: Data.Text.Text
  , uri :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Record where
  parseJSON = Data.Aeson.withObject "Record" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ Record cid uri value

instance Data.Aeson.ToJSON Record where
  toJSON (Record cid uri value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (Record cid uri value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

record'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Record -> kv
record'AesonFields (Record cid uri value) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]
