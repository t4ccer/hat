{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AspectRatio = AspectRatio
  { height :: Integer
  , width :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AspectRatio where
  parseJSON = Data.Aeson.withObject "AspectRatio" $ \v -> do
    height <- v Data.Aeson..: Data.Aeson.Key.fromString "height"
    width <- v Data.Aeson..: Data.Aeson.Key.fromString "width"
    pure $ AspectRatio height width

instance Data.Aeson.ToJSON AspectRatio where
  toJSON (AspectRatio height width) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]
  toEncoding (AspectRatio height width) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]

aspectRatio'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AspectRatio -> kv
aspectRatio'AesonFields (AspectRatio height width) =
  mconcat
    [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
    , Data.Aeson.Key.fromString "width" Data.Aeson..= width
    ]
