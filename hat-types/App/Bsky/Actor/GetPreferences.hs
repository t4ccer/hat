{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.GetPreferences where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetPreferencesResult = GetPreferencesResult
  { preferences :: App.Bsky.Actor.Defs.Preferences
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetPreferencesResult where
  parseJSON = Data.Aeson.withObject "GetPreferencesResult" $ \v -> do
    preferences <- v Data.Aeson..: Data.Aeson.Key.fromString "preferences"
    pure $ GetPreferencesResult preferences

instance Data.Aeson.ToJSON GetPreferencesResult where
  toJSON (GetPreferencesResult preferences) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "preferences" Data.Aeson..= preferences
        ]
  toEncoding (GetPreferencesResult preferences) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "preferences" Data.Aeson..= preferences
        ]

getPreferencesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetPreferencesResult -> kv
getPreferencesResult'AesonFields (GetPreferencesResult preferences) =
  mconcat
    [ Data.Aeson.Key.fromString "preferences" Data.Aeson..= preferences
    ]

type GetPreferences = "app.bsky.actor.getPreferences" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetPreferencesResult
