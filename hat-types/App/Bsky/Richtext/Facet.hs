{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Richtext.Facet where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ByteSlice = ByteSlice
  { byteEnd :: Integer
  , byteStart :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ByteSlice where
  parseJSON = Data.Aeson.withObject "ByteSlice" $ \v -> do
    byteEnd <- v Data.Aeson..: Data.Aeson.Key.fromString "byteEnd"
    byteStart <- v Data.Aeson..: Data.Aeson.Key.fromString "byteStart"
    pure $ ByteSlice byteEnd byteStart

instance Data.Aeson.ToJSON ByteSlice where
  toJSON (ByteSlice byteEnd byteStart) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "byteEnd" Data.Aeson..= byteEnd
        , Data.Aeson.Key.fromString "byteStart" Data.Aeson..= byteStart
        ]
  toEncoding (ByteSlice byteEnd byteStart) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "byteEnd" Data.Aeson..= byteEnd
        , Data.Aeson.Key.fromString "byteStart" Data.Aeson..= byteStart
        ]

byteSlice'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ByteSlice -> kv
byteSlice'AesonFields (ByteSlice byteEnd byteStart) =
  mconcat
    [ Data.Aeson.Key.fromString "byteEnd" Data.Aeson..= byteEnd
    , Data.Aeson.Key.fromString "byteStart" Data.Aeson..= byteStart
    ]

data Link = Link
  { uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Link where
  parseJSON = Data.Aeson.withObject "Link" $ \v -> do
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ Link uri

instance Data.Aeson.ToJSON Link where
  toJSON (Link uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (Link uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

link'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Link -> kv
link'AesonFields (Link uri) =
  mconcat
    [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data Facet = Facet
  { features :: [FacetFeaturesKind]
  , index :: ByteSlice
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Facet where
  parseJSON = Data.Aeson.withObject "Facet" $ \v -> do
    features <- v Data.Aeson..: Data.Aeson.Key.fromString "features"
    index <- v Data.Aeson..: Data.Aeson.Key.fromString "index"
    pure $ Facet features index

instance Data.Aeson.ToJSON Facet where
  toJSON (Facet features index) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "features" Data.Aeson..= features
        , Data.Aeson.Key.fromString "index" Data.Aeson..= index
        ]
  toEncoding (Facet features index) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "features" Data.Aeson..= features
        , Data.Aeson.Key.fromString "index" Data.Aeson..= index
        ]

facet'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Facet -> kv
facet'AesonFields (Facet features index) =
  mconcat
    [ Data.Aeson.Key.fromString "features" Data.Aeson..= features
    , Data.Aeson.Key.fromString "index" Data.Aeson..= index
    ]

data Mention = Mention
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Mention where
  parseJSON = Data.Aeson.withObject "Mention" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ Mention did

instance Data.Aeson.ToJSON Mention where
  toJSON (Mention did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (Mention did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

mention'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Mention -> kv
mention'AesonFields (Mention did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

data Tag = Tag
  { tag :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Tag where
  parseJSON = Data.Aeson.withObject "Tag" $ \v -> do
    tag <- v Data.Aeson..: Data.Aeson.Key.fromString "tag"
    pure $ Tag tag

instance Data.Aeson.ToJSON Tag where
  toJSON (Tag tag) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
        ]
  toEncoding (Tag tag) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
        ]

tag'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Tag -> kv
tag'AesonFields (Tag tag) =
  mconcat
    [ Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
    ]

data FacetFeaturesKind
  = FacetFeaturesKindMention Mention
  | FacetFeaturesKindLink Link
  | FacetFeaturesKindTag Tag
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FacetFeaturesKind where
  parseJSON = Data.Aeson.withObject "FacetFeaturesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.richtext.facet#mention" -> FacetFeaturesKindMention <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.richtext.facet#link" -> FacetFeaturesKindLink <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.richtext.facet#tag" -> FacetFeaturesKindTag <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON FacetFeaturesKind where
  toJSON = \case
    FacetFeaturesKindMention v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#mention") <> (App.Bsky.Richtext.Facet.mention'AesonFields v))
    FacetFeaturesKindLink v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#link") <> (App.Bsky.Richtext.Facet.link'AesonFields v))
    FacetFeaturesKindTag v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#tag") <> (App.Bsky.Richtext.Facet.tag'AesonFields v))
  toEncoding = \case
    FacetFeaturesKindMention v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#mention") <> (App.Bsky.Richtext.Facet.mention'AesonFields v))
    FacetFeaturesKindLink v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#link") <> (App.Bsky.Richtext.Facet.link'AesonFields v))
    FacetFeaturesKindTag v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.richtext.facet#tag") <> (App.Bsky.Richtext.Facet.tag'AesonFields v))
