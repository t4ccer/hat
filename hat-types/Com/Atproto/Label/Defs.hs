{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Label.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Label = Label
  { cid :: Maybe Data.Text.Text
  , cts :: Data.Text.Text
  , exp :: Maybe Data.Text.Text
  , neg :: Maybe Bool
  , sig :: Maybe Data.ByteString.ByteString
  , src :: Data.Text.Text
  , uri :: Data.Text.Text
  , val :: Data.Text.Text
  , ver :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Label where
  parseJSON = Data.Aeson.withObject "Label" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    cts <- v Data.Aeson..: Data.Aeson.Key.fromString "cts"
    exp <- v Data.Aeson..:? Data.Aeson.Key.fromString "exp"
    neg <- v Data.Aeson..:? Data.Aeson.Key.fromString "neg"
    sig <- fmap Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..:? Data.Aeson.Key.fromString "sig"
    src <- v Data.Aeson..: Data.Aeson.Key.fromString "src"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    val <- v Data.Aeson..: Data.Aeson.Key.fromString "val"
    ver <- v Data.Aeson..:? Data.Aeson.Key.fromString "ver"
    pure $ Label cid cts exp neg sig src uri val ver

instance Data.Aeson.ToJSON Label where
  toJSON (Label cid cts exp neg sig src uri val ver) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cts" Data.Aeson..= cts
        , Data.Aeson.Key.fromString "exp" Data.Aeson..?= exp
        , Data.Aeson.Key.fromString "neg" Data.Aeson..?= neg
        , Data.Aeson.Key.fromString "sig" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 sig
        , Data.Aeson.Key.fromString "src" Data.Aeson..= src
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "val" Data.Aeson..= val
        , Data.Aeson.Key.fromString "ver" Data.Aeson..?= ver
        ]
  toEncoding (Label cid cts exp neg sig src uri val ver) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "cts" Data.Aeson..= cts
        , Data.Aeson.Key.fromString "exp" Data.Aeson..?= exp
        , Data.Aeson.Key.fromString "neg" Data.Aeson..?= neg
        , Data.Aeson.Key.fromString "sig" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 sig
        , Data.Aeson.Key.fromString "src" Data.Aeson..= src
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "val" Data.Aeson..= val
        , Data.Aeson.Key.fromString "ver" Data.Aeson..?= ver
        ]

label'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Label -> kv
label'AesonFields (Label cid cts exp neg sig src uri val ver) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "cts" Data.Aeson..= cts
    , Data.Aeson.Key.fromString "exp" Data.Aeson..?= exp
    , Data.Aeson.Key.fromString "neg" Data.Aeson..?= neg
    , Data.Aeson.Key.fromString "sig" Data.Aeson..?= fmap Data.Text.Encoding.decodeUtf8 sig
    , Data.Aeson.Key.fromString "src" Data.Aeson..= src
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "val" Data.Aeson..= val
    , Data.Aeson.Key.fromString "ver" Data.Aeson..?= ver
    ]

newtype LabelValue = LabelValue
  { getLabelValue :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelValue where
  parseJSON = Data.Aeson.withText "LabelValue" $ pure . LabelValue

instance Data.Aeson.ToJSON LabelValue where
  toJSON (LabelValue getLabelValue) = Data.Aeson.toJSON getLabelValue
  toEncoding (LabelValue getLabelValue) = Data.Aeson.toEncoding getLabelValue

data LabelValueDefinition = LabelValueDefinition
  { adultOnly :: Maybe Bool
  , blurs :: Data.Text.Text
  , defaultSetting :: Maybe Data.Text.Text
  , identifier :: Data.Text.Text
  , locales :: [LabelValueDefinitionStrings]
  , severity :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelValueDefinition where
  parseJSON = Data.Aeson.withObject "LabelValueDefinition" $ \v -> do
    adultOnly <- v Data.Aeson..:? Data.Aeson.Key.fromString "adultOnly"
    blurs <- v Data.Aeson..: Data.Aeson.Key.fromString "blurs"
    defaultSetting <- v Data.Aeson..:? Data.Aeson.Key.fromString "defaultSetting"
    identifier <- v Data.Aeson..: Data.Aeson.Key.fromString "identifier"
    locales <- v Data.Aeson..: Data.Aeson.Key.fromString "locales"
    severity <- v Data.Aeson..: Data.Aeson.Key.fromString "severity"
    pure $ LabelValueDefinition adultOnly blurs defaultSetting identifier locales severity

instance Data.Aeson.ToJSON LabelValueDefinition where
  toJSON (LabelValueDefinition adultOnly blurs defaultSetting identifier locales severity) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "adultOnly" Data.Aeson..?= adultOnly
        , Data.Aeson.Key.fromString "blurs" Data.Aeson..= blurs
        , Data.Aeson.Key.fromString "defaultSetting" Data.Aeson..?= defaultSetting
        , Data.Aeson.Key.fromString "identifier" Data.Aeson..= identifier
        , Data.Aeson.Key.fromString "locales" Data.Aeson..= locales
        , Data.Aeson.Key.fromString "severity" Data.Aeson..= severity
        ]
  toEncoding (LabelValueDefinition adultOnly blurs defaultSetting identifier locales severity) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "adultOnly" Data.Aeson..?= adultOnly
        , Data.Aeson.Key.fromString "blurs" Data.Aeson..= blurs
        , Data.Aeson.Key.fromString "defaultSetting" Data.Aeson..?= defaultSetting
        , Data.Aeson.Key.fromString "identifier" Data.Aeson..= identifier
        , Data.Aeson.Key.fromString "locales" Data.Aeson..= locales
        , Data.Aeson.Key.fromString "severity" Data.Aeson..= severity
        ]

labelValueDefinition'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelValueDefinition -> kv
labelValueDefinition'AesonFields (LabelValueDefinition adultOnly blurs defaultSetting identifier locales severity) =
  mconcat
    [ Data.Aeson.Key.fromString "adultOnly" Data.Aeson..?= adultOnly
    , Data.Aeson.Key.fromString "blurs" Data.Aeson..= blurs
    , Data.Aeson.Key.fromString "defaultSetting" Data.Aeson..?= defaultSetting
    , Data.Aeson.Key.fromString "identifier" Data.Aeson..= identifier
    , Data.Aeson.Key.fromString "locales" Data.Aeson..= locales
    , Data.Aeson.Key.fromString "severity" Data.Aeson..= severity
    ]

data LabelValueDefinitionStrings = LabelValueDefinitionStrings
  { description :: Data.Text.Text
  , lang :: Data.Text.Text
  , name :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelValueDefinitionStrings where
  parseJSON = Data.Aeson.withObject "LabelValueDefinitionStrings" $ \v -> do
    description <- v Data.Aeson..: Data.Aeson.Key.fromString "description"
    lang <- v Data.Aeson..: Data.Aeson.Key.fromString "lang"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    pure $ LabelValueDefinitionStrings description lang name

instance Data.Aeson.ToJSON LabelValueDefinitionStrings where
  toJSON (LabelValueDefinitionStrings description lang name) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]
  toEncoding (LabelValueDefinitionStrings description lang name) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
        , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]

labelValueDefinitionStrings'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelValueDefinitionStrings -> kv
labelValueDefinitionStrings'AesonFields (LabelValueDefinitionStrings description lang name) =
  mconcat
    [ Data.Aeson.Key.fromString "description" Data.Aeson..= description
    , Data.Aeson.Key.fromString "lang" Data.Aeson..= lang
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    ]

data SelfLabel = SelfLabel
  { val :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SelfLabel where
  parseJSON = Data.Aeson.withObject "SelfLabel" $ \v -> do
    val <- v Data.Aeson..: Data.Aeson.Key.fromString "val"
    pure $ SelfLabel val

instance Data.Aeson.ToJSON SelfLabel where
  toJSON (SelfLabel val) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "val" Data.Aeson..= val
        ]
  toEncoding (SelfLabel val) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "val" Data.Aeson..= val
        ]

selfLabel'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SelfLabel -> kv
selfLabel'AesonFields (SelfLabel val) =
  mconcat
    [ Data.Aeson.Key.fromString "val" Data.Aeson..= val
    ]

data SelfLabels = SelfLabels
  { values :: [SelfLabel]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SelfLabels where
  parseJSON = Data.Aeson.withObject "SelfLabels" $ \v -> do
    values <- v Data.Aeson..: Data.Aeson.Key.fromString "values"
    pure $ SelfLabels values

instance Data.Aeson.ToJSON SelfLabels where
  toJSON (SelfLabels values) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "values" Data.Aeson..= values
        ]
  toEncoding (SelfLabels values) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "values" Data.Aeson..= values
        ]

selfLabels'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SelfLabels -> kv
selfLabels'AesonFields (SelfLabels values) =
  mconcat
    [ Data.Aeson.Key.fromString "values" Data.Aeson..= values
    ]
