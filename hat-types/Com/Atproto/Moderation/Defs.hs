{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Moderation.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

newtype ReasonType = ReasonType
  { getReasonType :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReasonType where
  parseJSON = Data.Aeson.withText "ReasonType" $ pure . ReasonType

instance Data.Aeson.ToJSON ReasonType where
  toJSON (ReasonType getReasonType) = Data.Aeson.toJSON getReasonType
  toEncoding (ReasonType getReasonType) = Data.Aeson.toEncoding getReasonType
