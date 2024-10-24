{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Video.GetJobStatus where

import {-# SOURCE #-} qualified App.Bsky.Video.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetJobStatusResult = GetJobStatusResult
  { jobStatus :: App.Bsky.Video.Defs.JobStatus
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetJobStatusResult where
  parseJSON = Data.Aeson.withObject "GetJobStatusResult" $ \v -> do
    jobStatus <- v Data.Aeson..: Data.Aeson.Key.fromString "jobStatus"
    pure $ GetJobStatusResult jobStatus

instance Data.Aeson.ToJSON GetJobStatusResult where
  toJSON (GetJobStatusResult jobStatus) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "jobStatus" Data.Aeson..= jobStatus
        ]
  toEncoding (GetJobStatusResult jobStatus) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "jobStatus" Data.Aeson..= jobStatus
        ]

getJobStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetJobStatusResult -> kv
getJobStatusResult'AesonFields (GetJobStatusResult jobStatus) =
  mconcat
    [ Data.Aeson.Key.fromString "jobStatus" Data.Aeson..= jobStatus
    ]

type GetJobStatus = "app.bsky.video.getJobStatus" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "jobId" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetJobStatusResult
