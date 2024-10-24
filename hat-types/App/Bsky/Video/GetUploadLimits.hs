{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Video.GetUploadLimits where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetUploadLimitsResult = GetUploadLimitsResult
  { canUpload :: Bool
  , error :: Maybe Data.Text.Text
  , message :: Maybe Data.Text.Text
  , remainingDailyBytes :: Maybe Integer
  , remainingDailyVideos :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetUploadLimitsResult where
  parseJSON = Data.Aeson.withObject "GetUploadLimitsResult" $ \v -> do
    canUpload <- v Data.Aeson..: Data.Aeson.Key.fromString "canUpload"
    error <- v Data.Aeson..:? Data.Aeson.Key.fromString "error"
    message <- v Data.Aeson..:? Data.Aeson.Key.fromString "message"
    remainingDailyBytes <- v Data.Aeson..:? Data.Aeson.Key.fromString "remainingDailyBytes"
    remainingDailyVideos <- v Data.Aeson..:? Data.Aeson.Key.fromString "remainingDailyVideos"
    pure $ GetUploadLimitsResult canUpload error message remainingDailyBytes remainingDailyVideos

instance Data.Aeson.ToJSON GetUploadLimitsResult where
  toJSON (GetUploadLimitsResult canUpload error message remainingDailyBytes remainingDailyVideos) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "canUpload" Data.Aeson..= canUpload
        , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
        , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "remainingDailyBytes" Data.Aeson..?= remainingDailyBytes
        , Data.Aeson.Key.fromString "remainingDailyVideos" Data.Aeson..?= remainingDailyVideos
        ]
  toEncoding (GetUploadLimitsResult canUpload error message remainingDailyBytes remainingDailyVideos) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "canUpload" Data.Aeson..= canUpload
        , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
        , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "remainingDailyBytes" Data.Aeson..?= remainingDailyBytes
        , Data.Aeson.Key.fromString "remainingDailyVideos" Data.Aeson..?= remainingDailyVideos
        ]

getUploadLimitsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetUploadLimitsResult -> kv
getUploadLimitsResult'AesonFields (GetUploadLimitsResult canUpload error message remainingDailyBytes remainingDailyVideos) =
  mconcat
    [ Data.Aeson.Key.fromString "canUpload" Data.Aeson..= canUpload
    , Data.Aeson.Key.fromString "error" Data.Aeson..?= error
    , Data.Aeson.Key.fromString "message" Data.Aeson..?= message
    , Data.Aeson.Key.fromString "remainingDailyBytes" Data.Aeson..?= remainingDailyBytes
    , Data.Aeson.Key.fromString "remainingDailyVideos" Data.Aeson..?= remainingDailyVideos
    ]

type GetUploadLimits = "app.bsky.video.getUploadLimits" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetUploadLimitsResult
