{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.StrongRef where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data StrongRef = StrongRef
  { cid :: Data.Text.Text
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON StrongRef where
  parseJSON = Data.Aeson.withObject "StrongRef" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ StrongRef cid uri

instance Data.Aeson.ToJSON StrongRef where
  toJSON (StrongRef cid uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (StrongRef cid uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

strongRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StrongRef -> kv
strongRef'AesonFields (StrongRef cid uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]
