{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Embed.External where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data External = External
  { description :: Data.Text.Text
  , thumb :: Maybe Data.ByteString.ByteString
  , title :: Data.Text.Text
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON External where
  parseJSON = Data.Aeson.withObject "External" $ \v -> do
    description <- v Data.Aeson..: Data.Aeson.Key.fromString "description"
    thumb <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "thumb"
    title <- v Data.Aeson..: Data.Aeson.Key.fromString "title"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ External description thumb title uri

instance Data.Aeson.ToJSON External where
  toJSON (External description thumb title uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 thumb
        , Data.Aeson.Key.fromString "title" Data.Aeson..= title
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (External description thumb title uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 thumb
        , Data.Aeson.Key.fromString "title" Data.Aeson..= title
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

external'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => External -> kv
external'AesonFields (External description thumb title uri) =
  mconcat
    [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
    , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 thumb
    , Data.Aeson.Key.fromString "title" Data.Aeson..= title
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data ExternalMain = ExternalMain
  { external :: External
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ExternalMain where
  parseJSON = Data.Aeson.withObject "ExternalMain" $ \v -> do
    external <- v Data.Aeson..: Data.Aeson.Key.fromString "external"
    pure $ ExternalMain external

instance Data.Aeson.ToJSON ExternalMain where
  toJSON (ExternalMain external) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
        ]
  toEncoding (ExternalMain external) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
        ]

externalMain'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ExternalMain -> kv
externalMain'AesonFields (ExternalMain external) =
  mconcat
    [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
    ]

data View = View
  { external :: ViewExternal
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON View where
  parseJSON = Data.Aeson.withObject "View" $ \v -> do
    external <- v Data.Aeson..: Data.Aeson.Key.fromString "external"
    pure $ View external

instance Data.Aeson.ToJSON View where
  toJSON (View external) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
        ]
  toEncoding (View external) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
        ]

view'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => View -> kv
view'AesonFields (View external) =
  mconcat
    [ Data.Aeson.Key.fromString "external" Data.Aeson..= external
    ]

data ViewExternal = ViewExternal
  { description :: Data.Text.Text
  , thumb :: Maybe Data.Text.Text
  , title :: Data.Text.Text
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewExternal where
  parseJSON = Data.Aeson.withObject "ViewExternal" $ \v -> do
    description <- v Data.Aeson..: Data.Aeson.Key.fromString "description"
    thumb <- v Data.Aeson..:? Data.Aeson.Key.fromString "thumb"
    title <- v Data.Aeson..: Data.Aeson.Key.fromString "title"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ ViewExternal description thumb title uri

instance Data.Aeson.ToJSON ViewExternal where
  toJSON (ViewExternal description thumb title uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= thumb
        , Data.Aeson.Key.fromString "title" Data.Aeson..= title
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (ViewExternal description thumb title uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= thumb
        , Data.Aeson.Key.fromString "title" Data.Aeson..= title
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

viewExternal'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewExternal -> kv
viewExternal'AesonFields (ViewExternal description thumb title uri) =
  mconcat
    [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
    , Data.Aeson.Key.fromString "thumb" Data.Aeson..?= thumb
    , Data.Aeson.Key.fromString "title" Data.Aeson..= title
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]
