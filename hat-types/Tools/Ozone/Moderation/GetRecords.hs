{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.GetRecords where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

data GetRecordsResult = GetRecordsResult
  { records :: [GetRecordsResultRecordsKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRecordsResult where
  parseJSON = Data.Aeson.withObject "GetRecordsResult" $ \v -> do
    records <- v Data.Aeson..: Data.Aeson.Key.fromString "records"
    pure $ GetRecordsResult records

instance Data.Aeson.ToJSON GetRecordsResult where
  toJSON (GetRecordsResult records) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "records" Data.Aeson..= records
        ]
  toEncoding (GetRecordsResult records) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "records" Data.Aeson..= records
        ]

getRecordsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecordsResult -> kv
getRecordsResult'AesonFields (GetRecordsResult records) =
  mconcat
    [ Data.Aeson.Key.fromString "records" Data.Aeson..= records
    ]

type GetRecords = "tools.ozone.moderation.getRecords" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "uris" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRecordsResult
data GetRecordsResultRecordsKind
  = GetRecordsResultRecordsKindRecordViewDetail Tools.Ozone.Moderation.Defs.RecordViewDetail
  | GetRecordsResultRecordsKindRecordViewNotFound Tools.Ozone.Moderation.Defs.RecordViewNotFound
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRecordsResultRecordsKind where
  parseJSON = Data.Aeson.withObject "GetRecordsResultRecordsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#recordViewDetail" -> GetRecordsResultRecordsKindRecordViewDetail <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#recordViewNotFound" -> GetRecordsResultRecordsKindRecordViewNotFound <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetRecordsResultRecordsKind where
  toJSON = \case
    GetRecordsResultRecordsKindRecordViewDetail v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewDetail") <> (Tools.Ozone.Moderation.Defs.recordViewDetail'AesonFields v))
    GetRecordsResultRecordsKindRecordViewNotFound v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewNotFound") <> (Tools.Ozone.Moderation.Defs.recordViewNotFound'AesonFields v))
  toEncoding = \case
    GetRecordsResultRecordsKindRecordViewDetail v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewDetail") <> (Tools.Ozone.Moderation.Defs.recordViewDetail'AesonFields v))
    GetRecordsResultRecordsKindRecordViewNotFound v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewNotFound") <> (Tools.Ozone.Moderation.Defs.recordViewNotFound'AesonFields v))
