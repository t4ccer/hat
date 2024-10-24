{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.Listitem where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Listitem = Listitem
  { createdAt :: Data.Text.Text
  , list :: Data.Text.Text
  , subject :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Listitem where
  parseJSON = Data.Aeson.withObject "Listitem" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    list <- v Data.Aeson..: Data.Aeson.Key.fromString "list"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ Listitem createdAt list subject

instance Data.Aeson.ToJSON Listitem where
  toJSON (Listitem createdAt list subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (Listitem createdAt list subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "list" Data.Aeson..= list
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

listitem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Listitem -> kv
listitem'AesonFields (Listitem createdAt list subject) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "list" Data.Aeson..= list
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]
