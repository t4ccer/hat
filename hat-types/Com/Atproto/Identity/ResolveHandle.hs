{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Identity.ResolveHandle where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ResolveHandleResult = ResolveHandleResult
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ResolveHandleResult where
  parseJSON = Data.Aeson.withObject "ResolveHandleResult" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ ResolveHandleResult did

instance Data.Aeson.ToJSON ResolveHandleResult where
  toJSON (ResolveHandleResult did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (ResolveHandleResult did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

resolveHandleResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ResolveHandleResult -> kv
resolveHandleResult'AesonFields (ResolveHandleResult did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

type ResolveHandle = "com.atproto.identity.resolveHandle" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "handle" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ResolveHandleResult
