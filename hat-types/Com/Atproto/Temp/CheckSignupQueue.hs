{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Temp.CheckSignupQueue where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data CheckSignupQueueResult = CheckSignupQueueResult
  { activated :: Bool
  , estimatedTimeMs :: Maybe Integer
  , placeInQueue :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON CheckSignupQueueResult where
  parseJSON = Data.Aeson.withObject "CheckSignupQueueResult" $ \v -> do
    activated <- v Data.Aeson..: Data.Aeson.Key.fromString "activated"
    estimatedTimeMs <- v Data.Aeson..:? Data.Aeson.Key.fromString "estimatedTimeMs"
    placeInQueue <- v Data.Aeson..:? Data.Aeson.Key.fromString "placeInQueue"
    pure $ CheckSignupQueueResult activated estimatedTimeMs placeInQueue

instance Data.Aeson.ToJSON CheckSignupQueueResult where
  toJSON (CheckSignupQueueResult activated estimatedTimeMs placeInQueue) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
        , Data.Aeson.Key.fromString "estimatedTimeMs" Data.Aeson..?= estimatedTimeMs
        , Data.Aeson.Key.fromString "placeInQueue" Data.Aeson..?= placeInQueue
        ]
  toEncoding (CheckSignupQueueResult activated estimatedTimeMs placeInQueue) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
        , Data.Aeson.Key.fromString "estimatedTimeMs" Data.Aeson..?= estimatedTimeMs
        , Data.Aeson.Key.fromString "placeInQueue" Data.Aeson..?= placeInQueue
        ]

checkSignupQueueResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CheckSignupQueueResult -> kv
checkSignupQueueResult'AesonFields (CheckSignupQueueResult activated estimatedTimeMs placeInQueue) =
  mconcat
    [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
    , Data.Aeson.Key.fromString "estimatedTimeMs" Data.Aeson..?= estimatedTimeMs
    , Data.Aeson.Key.fromString "placeInQueue" Data.Aeson..?= placeInQueue
    ]

type CheckSignupQueue = "com.atproto.temp.checkSignupQueue" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] CheckSignupQueueResult
