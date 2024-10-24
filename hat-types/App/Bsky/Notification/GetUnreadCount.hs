{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Notification.GetUnreadCount where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetUnreadCountResult = GetUnreadCountResult
  { count :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetUnreadCountResult where
  parseJSON = Data.Aeson.withObject "GetUnreadCountResult" $ \v -> do
    count <- v Data.Aeson..: Data.Aeson.Key.fromString "count"
    pure $ GetUnreadCountResult count

instance Data.Aeson.ToJSON GetUnreadCountResult where
  toJSON (GetUnreadCountResult count) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
        ]
  toEncoding (GetUnreadCountResult count) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
        ]

getUnreadCountResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetUnreadCountResult -> kv
getUnreadCountResult'AesonFields (GetUnreadCountResult count) =
  mconcat
    [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
    ]

type GetUnreadCount = "app.bsky.notification.getUnreadCount" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "priority" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "seenAt" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetUnreadCountResult
