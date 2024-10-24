{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Set.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Set = Set
  { description :: Maybe Data.Text.Text
  , name :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Set where
  parseJSON = Data.Aeson.withObject "Set" $ \v -> do
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    pure $ Set description name

instance Data.Aeson.ToJSON Set where
  toJSON (Set description name) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]
  toEncoding (Set description name) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]

set'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Set -> kv
set'AesonFields (Set description name) =
  mconcat
    [ Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    ]

data SetView = SetView
  { createdAt :: Data.Text.Text
  , description :: Maybe Data.Text.Text
  , name :: Data.Text.Text
  , setSize :: Integer
  , updatedAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SetView where
  parseJSON = Data.Aeson.withObject "SetView" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    setSize <- v Data.Aeson..: Data.Aeson.Key.fromString "setSize"
    updatedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "updatedAt"
    pure $ SetView createdAt description name setSize updatedAt

instance Data.Aeson.ToJSON SetView where
  toJSON (SetView createdAt description name setSize updatedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "setSize" Data.Aeson..= setSize
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]
  toEncoding (SetView createdAt description name setSize updatedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "setSize" Data.Aeson..= setSize
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]

setView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SetView -> kv
setView'AesonFields (SetView createdAt description name setSize updatedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "setSize" Data.Aeson..= setSize
    , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
    ]
