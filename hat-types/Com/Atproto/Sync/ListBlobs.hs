{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.ListBlobs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListBlobsResult = ListBlobsResult
  { cids :: [Data.Text.Text]
  , cursor :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListBlobsResult where
  parseJSON = Data.Aeson.withObject "ListBlobsResult" $ \v -> do
    cids <- v Data.Aeson..: Data.Aeson.Key.fromString "cids"
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    pure $ ListBlobsResult cids cursor

instance Data.Aeson.ToJSON ListBlobsResult where
  toJSON (ListBlobsResult cids cursor) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cids" Data.Aeson..= cids
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]
  toEncoding (ListBlobsResult cids cursor) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cids" Data.Aeson..= cids
        , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        ]

listBlobsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListBlobsResult -> kv
listBlobsResult'AesonFields (ListBlobsResult cids cursor) =
  mconcat
    [ Data.Aeson.Key.fromString "cids" Data.Aeson..= cids
    , Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    ]

type ListBlobs = "com.atproto.sync.listBlobs" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "since" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListBlobsResult
