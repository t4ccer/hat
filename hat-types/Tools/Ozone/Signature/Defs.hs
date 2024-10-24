{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Signature.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SigDetail = SigDetail
  { property :: Data.Text.Text
  , value :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SigDetail where
  parseJSON = Data.Aeson.withObject "SigDetail" $ \v -> do
    property <- v Data.Aeson..: Data.Aeson.Key.fromString "property"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ SigDetail property value

instance Data.Aeson.ToJSON SigDetail where
  toJSON (SigDetail property value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "property" Data.Aeson..= property
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (SigDetail property value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "property" Data.Aeson..= property
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

sigDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SigDetail -> kv
sigDetail'AesonFields (SigDetail property value) =
  mconcat
    [ Data.Aeson.Key.fromString "property" Data.Aeson..= property
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]
