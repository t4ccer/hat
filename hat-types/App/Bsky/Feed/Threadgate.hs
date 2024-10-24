{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Threadgate where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data FollowingRule = FollowingRule
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FollowingRule where
  parseJSON = Data.Aeson.withObject "FollowingRule" $ \v -> do
    pure $ FollowingRule

instance Data.Aeson.ToJSON FollowingRule where
  toJSON (FollowingRule) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (FollowingRule) =
    Data.Aeson.pairs $
      mconcat
        []

followingRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FollowingRule -> kv
followingRule'AesonFields (FollowingRule) =
  mconcat
    []

data ListRule = ListRule
  { list :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListRule where
  parseJSON = Data.Aeson.withObject "ListRule" $ \v -> do
    list <- v Data.Aeson..: Data.Aeson.Key.fromString "list"
    pure $ ListRule list

instance Data.Aeson.ToJSON ListRule where
  toJSON (ListRule list) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "list" Data.Aeson..= list
        ]
  toEncoding (ListRule list) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "list" Data.Aeson..= list
        ]

listRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListRule -> kv
listRule'AesonFields (ListRule list) =
  mconcat
    [ Data.Aeson.Key.fromString "list" Data.Aeson..= list
    ]

data Threadgate = Threadgate
  { allow :: Maybe [ThreadgateAllowKind]
  , createdAt :: Data.Text.Text
  , hiddenReplies :: Maybe [Data.Text.Text]
  , post :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Threadgate where
  parseJSON = Data.Aeson.withObject "Threadgate" $ \v -> do
    allow <- v Data.Aeson..:? Data.Aeson.Key.fromString "allow"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    hiddenReplies <- v Data.Aeson..:? Data.Aeson.Key.fromString "hiddenReplies"
    post <- v Data.Aeson..: Data.Aeson.Key.fromString "post"
    pure $ Threadgate allow createdAt hiddenReplies post

instance Data.Aeson.ToJSON Threadgate where
  toJSON (Threadgate allow createdAt hiddenReplies post) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "allow" Data.Aeson..?= allow
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "hiddenReplies" Data.Aeson..?= hiddenReplies
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        ]
  toEncoding (Threadgate allow createdAt hiddenReplies post) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "allow" Data.Aeson..?= allow
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "hiddenReplies" Data.Aeson..?= hiddenReplies
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        ]

threadgate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Threadgate -> kv
threadgate'AesonFields (Threadgate allow createdAt hiddenReplies post) =
  mconcat
    [ Data.Aeson.Key.fromString "allow" Data.Aeson..?= allow
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "hiddenReplies" Data.Aeson..?= hiddenReplies
    , Data.Aeson.Key.fromString "post" Data.Aeson..= post
    ]

data MentionRule = MentionRule
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MentionRule where
  parseJSON = Data.Aeson.withObject "MentionRule" $ \v -> do
    pure $ MentionRule

instance Data.Aeson.ToJSON MentionRule where
  toJSON (MentionRule) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (MentionRule) =
    Data.Aeson.pairs $
      mconcat
        []

mentionRule'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MentionRule -> kv
mentionRule'AesonFields (MentionRule) =
  mconcat
    []

data ThreadgateAllowKind
  = ThreadgateAllowKindMentionRule MentionRule
  | ThreadgateAllowKindFollowingRule FollowingRule
  | ThreadgateAllowKindListRule ListRule
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadgateAllowKind where
  parseJSON = Data.Aeson.withObject "ThreadgateAllowKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.threadgate#mentionRule" -> ThreadgateAllowKindMentionRule <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.threadgate#followingRule" -> ThreadgateAllowKindFollowingRule <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.threadgate#listRule" -> ThreadgateAllowKindListRule <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ThreadgateAllowKind where
  toJSON = \case
    ThreadgateAllowKindMentionRule v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#mentionRule") <> (App.Bsky.Feed.Threadgate.mentionRule'AesonFields v))
    ThreadgateAllowKindFollowingRule v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#followingRule") <> (App.Bsky.Feed.Threadgate.followingRule'AesonFields v))
    ThreadgateAllowKindListRule v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#listRule") <> (App.Bsky.Feed.Threadgate.listRule'AesonFields v))
  toEncoding = \case
    ThreadgateAllowKindMentionRule v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#mentionRule") <> (App.Bsky.Feed.Threadgate.mentionRule'AesonFields v))
    ThreadgateAllowKindFollowingRule v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#followingRule") <> (App.Bsky.Feed.Threadgate.followingRule'AesonFields v))
    ThreadgateAllowKindListRule v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.threadgate#listRule") <> (App.Bsky.Feed.Threadgate.listRule'AesonFields v))
