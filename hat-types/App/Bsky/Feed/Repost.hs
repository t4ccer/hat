{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Repost where

import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Repost = Repost
  { createdAt :: Data.Text.Text
  , subject :: Com.Atproto.Repo.StrongRef.StrongRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Repost where
  parseJSON = Data.Aeson.withObject "Repost" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ Repost createdAt subject

instance Data.Aeson.ToJSON Repost where
  toJSON (Repost createdAt subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (Repost createdAt subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

repost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Repost -> kv
repost'AesonFields (Repost createdAt subject) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]
