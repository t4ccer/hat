{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.GetLatestCommit where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetLatestCommitResult = GetLatestCommitResult
  { cid :: Data.Text.Text
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetLatestCommitResult where
  parseJSON = Data.Aeson.withObject "GetLatestCommitResult" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ GetLatestCommitResult cid rev

instance Data.Aeson.ToJSON GetLatestCommitResult where
  toJSON (GetLatestCommitResult cid rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (GetLatestCommitResult cid rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

getLatestCommitResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetLatestCommitResult -> kv
getLatestCommitResult'AesonFields (GetLatestCommitResult cid rev) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]

type GetLatestCommit = "com.atproto.sync.getLatestCommit" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetLatestCommitResult
