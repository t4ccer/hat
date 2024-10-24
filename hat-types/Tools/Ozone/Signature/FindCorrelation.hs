{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Signature.FindCorrelation where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Signature.Defs

data FindCorrelationResult = FindCorrelationResult
  { details :: [Tools.Ozone.Signature.Defs.SigDetail]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FindCorrelationResult where
  parseJSON = Data.Aeson.withObject "FindCorrelationResult" $ \v -> do
    details <- v Data.Aeson..: Data.Aeson.Key.fromString "details"
    pure $ FindCorrelationResult details

instance Data.Aeson.ToJSON FindCorrelationResult where
  toJSON (FindCorrelationResult details) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "details" Data.Aeson..= details
        ]
  toEncoding (FindCorrelationResult details) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "details" Data.Aeson..= details
        ]

findCorrelationResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FindCorrelationResult -> kv
findCorrelationResult'AesonFields (FindCorrelationResult details) =
  mconcat
    [ Data.Aeson.Key.fromString "details" Data.Aeson..= details
    ]

type FindCorrelation = "tools.ozone.signature.findCorrelation" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "dids" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] FindCorrelationResult
