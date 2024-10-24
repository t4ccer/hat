{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Chat.Bsky.Actor.Declaration where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Declaration = Declaration
  { allowIncoming :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Declaration where
  parseJSON = Data.Aeson.withObject "Declaration" $ \v -> do
    allowIncoming <- v Data.Aeson..: Data.Aeson.Key.fromString "allowIncoming"
    pure $ Declaration allowIncoming

instance Data.Aeson.ToJSON Declaration where
  toJSON (Declaration allowIncoming) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
        ]
  toEncoding (Declaration allowIncoming) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
        ]

declaration'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Declaration -> kv
declaration'AesonFields (Declaration allowIncoming) =
  mconcat
    [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
    ]
