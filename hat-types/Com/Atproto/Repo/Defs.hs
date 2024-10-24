{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data CommitMeta = CommitMeta
  { cid :: Data.Text.Text
  , rev :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON CommitMeta where
  parseJSON = Data.Aeson.withObject "CommitMeta" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    pure $ CommitMeta cid rev

instance Data.Aeson.ToJSON CommitMeta where
  toJSON (CommitMeta cid rev) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]
  toEncoding (CommitMeta cid rev) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        ]

commitMeta'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CommitMeta -> kv
commitMeta'AesonFields (CommitMeta cid rev) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    ]
