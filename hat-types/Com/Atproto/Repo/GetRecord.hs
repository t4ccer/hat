{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.GetRecord where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetRecordResult = GetRecordResult
  { cid :: Maybe Data.Text.Text
  , uri :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRecordResult where
  parseJSON = Data.Aeson.withObject "GetRecordResult" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ GetRecordResult cid uri value

instance Data.Aeson.ToJSON GetRecordResult where
  toJSON (GetRecordResult cid uri value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (GetRecordResult cid uri value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

getRecordResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecordResult -> kv
getRecordResult'AesonFields (GetRecordResult cid uri value) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

type GetRecord = "com.atproto.repo.getRecord" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cid" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "collection" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "repo" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "rkey" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRecordResult
