{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Like where

import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Like = Like
  { createdAt :: Data.Text.Text
  , subject :: Com.Atproto.Repo.StrongRef.StrongRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Like where
  parseJSON = Data.Aeson.withObject "Like" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ Like createdAt subject

instance Data.Aeson.ToJSON Like where
  toJSON (Like createdAt subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (Like createdAt subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

like'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Like -> kv
like'AesonFields (Like createdAt subject) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]
