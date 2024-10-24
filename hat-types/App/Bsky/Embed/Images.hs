{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.Images where

import {-# SOURCE #-} qualified App.Bsky.Embed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Image = Image
  { alt :: Data.Text.Text
  , aspectRatio :: Maybe App.Bsky.Embed.Defs.AspectRatio
  , image :: Data.ByteString.ByteString
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Image where
  parseJSON = Data.Aeson.withObject "Image" $ \v -> do
    alt <- v Data.Aeson..: Data.Aeson.Key.fromString "alt"
    aspectRatio <- v Data.Aeson..:? Data.Aeson.Key.fromString "aspectRatio"
    image <- Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..: Data.Aeson.Key.fromString "image"
    pure $ Image alt aspectRatio image

instance Data.Aeson.ToJSON Image where
  toJSON (Image alt aspectRatio image) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "image" Data.Aeson..= Data.Text.Encoding.decodeUtf8 image
        ]
  toEncoding (Image alt aspectRatio image) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "image" Data.Aeson..= Data.Text.Encoding.decodeUtf8 image
        ]

image'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Image -> kv
image'AesonFields (Image alt aspectRatio image) =
  mconcat
    [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
    , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
    , Data.Aeson.Key.fromString "image" Data.Aeson..= Data.Text.Encoding.decodeUtf8 image
    ]

data Images = Images
  { images :: [Image]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Images where
  parseJSON = Data.Aeson.withObject "Images" $ \v -> do
    images <- v Data.Aeson..: Data.Aeson.Key.fromString "images"
    pure $ Images images

instance Data.Aeson.ToJSON Images where
  toJSON (Images images) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
        ]
  toEncoding (Images images) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
        ]

images'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Images -> kv
images'AesonFields (Images images) =
  mconcat
    [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
    ]

data View = View
  { images :: [ViewImage]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON View where
  parseJSON = Data.Aeson.withObject "View" $ \v -> do
    images <- v Data.Aeson..: Data.Aeson.Key.fromString "images"
    pure $ View images

instance Data.Aeson.ToJSON View where
  toJSON (View images) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
        ]
  toEncoding (View images) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
        ]

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
view'AesonFields (View images) =
  mconcat
    [ Data.Aeson.Key.fromString "images" Data.Aeson..= images
    ]

data ViewImage = ViewImage
  { alt :: Data.Text.Text
  , aspectRatio :: Maybe App.Bsky.Embed.Defs.AspectRatio
  , fullsize :: Data.Text.Text
  , thumb :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewImage where
  parseJSON = Data.Aeson.withObject "ViewImage" $ \v -> do
    alt <- v Data.Aeson..: Data.Aeson.Key.fromString "alt"
    aspectRatio <- v Data.Aeson..:? Data.Aeson.Key.fromString "aspectRatio"
    fullsize <- v Data.Aeson..: Data.Aeson.Key.fromString "fullsize"
    thumb <- v Data.Aeson..: Data.Aeson.Key.fromString "thumb"
    pure $ ViewImage alt aspectRatio fullsize thumb

instance Data.Aeson.ToJSON ViewImage where
  toJSON (ViewImage alt aspectRatio fullsize thumb) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "fullsize" Data.Aeson..= fullsize
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..= thumb
        ]
  toEncoding (ViewImage alt aspectRatio fullsize thumb) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "fullsize" Data.Aeson..= fullsize
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..= thumb
        ]

viewImage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewImage -> kv
viewImage'AesonFields (ViewImage alt aspectRatio fullsize thumb) =
  mconcat
    [ Data.Aeson.Key.fromString "alt" Data.Aeson..= alt
    , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
    , Data.Aeson.Key.fromString "fullsize" Data.Aeson..= fullsize
    , Data.Aeson.Key.fromString "thumb" Data.Aeson..= thumb
    ]
