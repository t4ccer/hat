{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.Video where

import {-# SOURCE #-} qualified App.Bsky.Embed.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Caption = Caption
  { file :: Data.ByteString.ByteString
  , lang :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Caption where
  parseJSON = Data.Aeson.withObject "Caption" $ \v -> do
    file <- Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..: Data.Aeson.Key.fromString "file"
    lang <- v Data.Aeson..: Data.Aeson.Key.fromString "lang"
    pure $ Caption file lang

instance Data.Aeson.ToJSON Caption where
  toJSON (Caption file lang) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "file" Data.Aeson..= Data.Text.Encoding.decodeUtf8 file
        , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
        ]
  toEncoding (Caption file lang) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "file" Data.Aeson..= Data.Text.Encoding.decodeUtf8 file
        , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
        ]

caption'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Caption -> kv
caption'AesonFields (Caption file lang) =
  mconcat
    [ Data.Aeson.Key.fromString "file" Data.Aeson..= Data.Text.Encoding.decodeUtf8 file
    , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
    ]

data Video = Video
  { alt :: Maybe Data.Text.Text
  , aspectRatio :: Maybe App.Bsky.Embed.Defs.AspectRatio
  , captions :: Maybe [Caption]
  , video :: Data.ByteString.ByteString
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Video where
  parseJSON = Data.Aeson.withObject "Video" $ \v -> do
    alt <- v Data.Aeson..:? Data.Aeson.Key.fromString "alt"
    aspectRatio <- v Data.Aeson..:? Data.Aeson.Key.fromString "aspectRatio"
    captions <- v Data.Aeson..:? Data.Aeson.Key.fromString "captions"
    video <- Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..: Data.Aeson.Key.fromString "video"
    pure $ Video alt aspectRatio captions video

instance Data.Aeson.ToJSON Video where
  toJSON (Video alt aspectRatio captions video) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "captions" Data.Aeson..?= captions
        , Data.Aeson.Key.fromString "video" Data.Aeson..= Data.Text.Encoding.decodeUtf8 video
        ]
  toEncoding (Video alt aspectRatio captions video) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "captions" Data.Aeson..?= captions
        , Data.Aeson.Key.fromString "video" Data.Aeson..= Data.Text.Encoding.decodeUtf8 video
        ]

video'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Video -> kv
video'AesonFields (Video alt aspectRatio captions video) =
  mconcat
    [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
    , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
    , Data.Aeson.Key.fromString "captions" Data.Aeson..?= captions
    , Data.Aeson.Key.fromString "video" Data.Aeson..= Data.Text.Encoding.decodeUtf8 video
    ]

data View = View
  { alt :: Maybe Data.Text.Text
  , aspectRatio :: Maybe App.Bsky.Embed.Defs.AspectRatio
  , cid :: Data.Text.Text
  , playlist :: Data.Text.Text
  , thumbnail :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON View where
  parseJSON = Data.Aeson.withObject "View" $ \v -> do
    alt <- v Data.Aeson..:? Data.Aeson.Key.fromString "alt"
    aspectRatio <- v Data.Aeson..:? Data.Aeson.Key.fromString "aspectRatio"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    playlist <- v Data.Aeson..: Data.Aeson.Key.fromString "playlist"
    thumbnail <- v Data.Aeson..:? Data.Aeson.Key.fromString "thumbnail"
    pure $ View alt aspectRatio cid playlist thumbnail

instance Data.Aeson.ToJSON View where
  toJSON (View alt aspectRatio cid playlist thumbnail) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "playlist" Data.Aeson..= playlist
        , Data.Aeson.Key.fromString "thumbnail" Data.Aeson..?= thumbnail
        ]
  toEncoding (View alt aspectRatio cid playlist thumbnail) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
        , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "playlist" Data.Aeson..= playlist
        , Data.Aeson.Key.fromString "thumbnail" Data.Aeson..?= thumbnail
        ]

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
view'AesonFields (View alt aspectRatio cid playlist thumbnail) =
  mconcat
    [ Data.Aeson.Key.fromString "alt" Data.Aeson..?= alt
    , Data.Aeson.Key.fromString "aspectRatio" Data.Aeson..?= aspectRatio
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "playlist" Data.Aeson..= playlist
    , Data.Aeson.Key.fromString "thumbnail" Data.Aeson..?= thumbnail
    ]
