{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Postgate where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data DisableRule = DisableRule
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DisableRule where
  parseJSON = Data.Aeson.withObject "DisableRule" $ \v -> do
    pure $ DisableRule

instance Data.Aeson.ToJSON DisableRule where
  toJSON (DisableRule) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (DisableRule) =
    Data.Aeson.pairs $
      mconcat
        []

disableRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DisableRule -> kv
disableRule'AesonFields (DisableRule) =
  mconcat
    []

data Postgate = Postgate
  { createdAt :: Data.Text.Text
  , detachedEmbeddingUris :: Maybe [Data.Text.Text]
  , embeddingRules :: Maybe [PostgateEmbeddingRulesKind]
  , post :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Postgate where
  parseJSON = Data.Aeson.withObject "Postgate" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    detachedEmbeddingUris <- v Data.Aeson..:? Data.Aeson.Key.fromString "detachedEmbeddingUris"
    embeddingRules <- v Data.Aeson..:? Data.Aeson.Key.fromString "embeddingRules"
    post <- v Data.Aeson..: Data.Aeson.Key.fromString "post"
    pure $ Postgate createdAt detachedEmbeddingUris embeddingRules post

instance Data.Aeson.ToJSON Postgate where
  toJSON (Postgate createdAt detachedEmbeddingUris embeddingRules post) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "detachedEmbeddingUris" Data.Aeson..?= detachedEmbeddingUris
        , Data.Aeson.Key.fromString "embeddingRules" Data.Aeson..?= embeddingRules
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        ]
  toEncoding (Postgate createdAt detachedEmbeddingUris embeddingRules post) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "detachedEmbeddingUris" Data.Aeson..?= detachedEmbeddingUris
        , Data.Aeson.Key.fromString "embeddingRules" Data.Aeson..?= embeddingRules
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        ]

postgate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Postgate -> kv
postgate'AesonFields (Postgate createdAt detachedEmbeddingUris embeddingRules post) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "detachedEmbeddingUris" Data.Aeson..?= detachedEmbeddingUris
    , Data.Aeson.Key.fromString "embeddingRules" Data.Aeson..?= embeddingRules
    , Data.Aeson.Key.fromString "post" Data.Aeson..= post
    ]

data PostgateEmbeddingRulesKind
  = PostgateEmbeddingRulesKindDisableRule DisableRule
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PostgateEmbeddingRulesKind where
  parseJSON = Data.Aeson.withObject "PostgateEmbeddingRulesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.postgate#disableRule" -> PostgateEmbeddingRulesKindDisableRule <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON PostgateEmbeddingRulesKind where
  toJSON = \case
    PostgateEmbeddingRulesKindDisableRule v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.postgate#disableRule") <> (App.Bsky.Feed.Postgate.disableRule'AesonFields v))
  toEncoding = \case
    PostgateEmbeddingRulesKindDisableRule v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.postgate#disableRule") <> (App.Bsky.Feed.Postgate.disableRule'AesonFields v))
