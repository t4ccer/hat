{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Notification.ListNotifications where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListNotificationsResult = ListNotificationsResult
  { cursor :: Maybe Data.Text.Text
  , notifications :: [Notification]
  , priority :: Maybe Bool
  , seenAt :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListNotificationsResult where
  parseJSON = Data.Aeson.withObject "ListNotificationsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    notifications <- v Data.Aeson..: Data.Aeson.Key.fromString "notifications"
    priority <- v Data.Aeson..:? Data.Aeson.Key.fromString "priority"
    seenAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "seenAt"
    pure $ ListNotificationsResult cursor notifications priority seenAt

instance Data.Aeson.ToJSON ListNotificationsResult where
  toJSON (ListNotificationsResult cursor notifications priority seenAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "notifications" Data.Aeson..= notifications
        , Data.Aeson.Key.fromString "priority" Data.Aeson..?= priority
        , Data.Aeson.Key.fromString "seenAt" Data.Aeson..?= seenAt
        ]
  toEncoding (ListNotificationsResult cursor notifications priority seenAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "notifications" Data.Aeson..= notifications
        , Data.Aeson.Key.fromString "priority" Data.Aeson..?= priority
        , Data.Aeson.Key.fromString "seenAt" Data.Aeson..?= seenAt
        ]

listNotificationsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListNotificationsResult -> kv
listNotificationsResult'AesonFields (ListNotificationsResult cursor notifications priority seenAt) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "notifications" Data.Aeson..= notifications
    , Data.Aeson.Key.fromString "priority" Data.Aeson..?= priority
    , Data.Aeson.Key.fromString "seenAt" Data.Aeson..?= seenAt
    ]

type ListNotifications = "app.bsky.notification.listNotifications" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "priority" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "seenAt" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListNotificationsResult
data Notification = Notification
  { author :: App.Bsky.Actor.Defs.ProfileView
  , cid :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , isRead :: Bool
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , reason :: Data.Text.Text
  , reasonSubject :: Maybe Data.Text.Text
  , record :: Data.Aeson.Value
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Notification where
  parseJSON = Data.Aeson.withObject "Notification" $ \v -> do
    author <- v Data.Aeson..: Data.Aeson.Key.fromString "author"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    isRead <- v Data.Aeson..: Data.Aeson.Key.fromString "isRead"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    reason <- v Data.Aeson..: Data.Aeson.Key.fromString "reason"
    reasonSubject <- v Data.Aeson..:? Data.Aeson.Key.fromString "reasonSubject"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ Notification author cid indexedAt isRead labels reason reasonSubject record uri

instance Data.Aeson.ToJSON Notification where
  toJSON (Notification author cid indexedAt isRead labels reason reasonSubject record uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "isRead" Data.Aeson..= isRead
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "reason" Data.Aeson..= reason
        , Data.Aeson.Key.fromString "reasonSubject" Data.Aeson..?= reasonSubject
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (Notification author cid indexedAt isRead labels reason reasonSubject record uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "isRead" Data.Aeson..= isRead
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "reason" Data.Aeson..= reason
        , Data.Aeson.Key.fromString "reasonSubject" Data.Aeson..?= reasonSubject
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

notification'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Notification -> kv
notification'AesonFields (Notification author cid indexedAt isRead labels reason reasonSubject record uri) =
  mconcat
    [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "isRead" Data.Aeson..= isRead
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "reason" Data.Aeson..= reason
    , Data.Aeson.Key.fromString "reasonSubject" Data.Aeson..?= reasonSubject
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]
