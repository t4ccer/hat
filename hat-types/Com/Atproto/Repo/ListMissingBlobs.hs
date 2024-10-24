{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.ListMissingBlobs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListMissingBlobsResult = ListMissingBlobsResult
  { blobs :: [RecordBlob]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListMissingBlobsResult where
  parseJSON = Data.Aeson.withObject "ListMissingBlobsResult" $ \v -> do
    blobs <- v Data.Aeson..: Data.Aeson.Key.fromString "blobs"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ ListMissingBlobsResult blobs cursor

instance Data.Aeson.ToJSON ListMissingBlobsResult where
  toJSON (ListMissingBlobsResult blobs cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (ListMissingBlobsResult blobs cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

listMissingBlobsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListMissingBlobsResult -> kv
listMissingBlobsResult'AesonFields (ListMissingBlobsResult blobs cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type ListMissingBlobs = "com.atproto.repo.listMissingBlobs" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListMissingBlobsResult
data RecordBlob = RecordBlob
  { cid :: Data.Text.Text
  , recordUri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordBlob where
  parseJSON = Data.Aeson.withObject "RecordBlob" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    recordUri <- v Data.Aeson..: Data.Aeson.Key.fromString "recordUri"
    pure $ RecordBlob cid recordUri

instance Data.Aeson.ToJSON RecordBlob where
  toJSON (RecordBlob cid recordUri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "recordUri" Data.Aeson..= recordUri
        ]
  toEncoding (RecordBlob cid recordUri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "recordUri" Data.Aeson..= recordUri
        ]

recordBlob'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordBlob -> kv
recordBlob'AesonFields (RecordBlob cid recordUri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "recordUri" Data.Aeson..= recordUri
    ]
