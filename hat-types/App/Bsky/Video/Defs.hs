{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Video.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data JobStatus = JobStatus
  { blob :: Maybe Data.ByteString.ByteString
  , did :: Data.Text.Text
  , error :: Maybe Data.Text.Text
  , jobId :: Data.Text.Text
  , message :: Maybe Data.Text.Text
  , progress :: Maybe Integer
  , state :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON JobStatus where
  parseJSON = Data.Aeson.withObject "JobStatus" $ \v -> do
    blob <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "blob"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    error <- v Data.Aeson..:? Data.Aeson.Key.fromString "error"
    jobId <- v Data.Aeson..: Data.Aeson.Key.fromString "jobId"
    message <- v Data.Aeson..:? Data.Aeson.Key.fromString "message"
    progress <- v Data.Aeson..:? Data.Aeson.Key.fromString "progress"
    state <- v Data.Aeson..: Data.Aeson.Key.fromString "state"
    pure $ JobStatus blob did error jobId message progress state

instance Data.Aeson.ToJSON JobStatus where
  toJSON (JobStatus blob did error jobId message progress state) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blob" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 blob
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
        , Data.Aeson.Key.fromString "jobId" Data.Aeson..= jobId
        , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "progress" Data.Aeson..?= progress
        , Data.Aeson.Key.fromString "state" Data.Aeson..= state
        ]
  toEncoding (JobStatus blob did error jobId message progress state) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blob" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 blob
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
        , Data.Aeson.Key.fromString "jobId" Data.Aeson..= jobId
        , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "progress" Data.Aeson..?= progress
        , Data.Aeson.Key.fromString "state" Data.Aeson..= state
        ]

jobStatus'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => JobStatus -> kv
jobStatus'AesonFields (JobStatus blob did error jobId message progress state) =
  mconcat
    [ Data.Aeson.Key.fromString "blob" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 blob
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
    , Data.Aeson.Key.fromString "jobId" Data.Aeson..= jobId
    , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
    , Data.Aeson.Key.fromString "progress" Data.Aeson..?= progress
    , Data.Aeson.Key.fromString "state" Data.Aeson..= state
    ]
