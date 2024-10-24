{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.Block where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Block = Block
  { createdAt :: Data.Text.Text
  , subject :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Block where
  parseJSON = Data.Aeson.withObject "Block" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    pure $ Block createdAt subject

instance Data.Aeson.ToJSON Block where
  toJSON (Block createdAt subject) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]
  toEncoding (Block createdAt subject) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        ]

block'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Block -> kv
block'AesonFields (Block createdAt subject) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    ]
