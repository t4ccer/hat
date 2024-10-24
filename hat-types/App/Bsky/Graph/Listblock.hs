{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.Listblock where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Listblock = Listblock
  { createdAt :: Data.Text.Text
  , subject :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Listblock where
  parseJSON = Data.Aeson.withObject "Listblock" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ Listblock createdAt subject

instance Data.Aeson.ToJSON Listblock where
  toJSON (Listblock createdAt subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (Listblock createdAt subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

listblock'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Listblock -> kv
listblock'AesonFields (Listblock createdAt subject) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]
