{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Communication.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data TemplateView = TemplateView
  { contentMarkdown :: Data.Text.Text
  , createdAt :: Data.Text.Text
  , disabled :: Bool
  , id' :: Data.Text.Text
  , lang :: Maybe Data.Text.Text
  , lastUpdatedBy :: Data.Text.Text
  , name :: Data.Text.Text
  , subject :: Maybe Data.Text.Text
  , updatedAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON TemplateView where
  parseJSON = Data.Aeson.withObject "TemplateView" $ \v -> do
    contentMarkdown <- v Data.Aeson..: Data.Aeson.Key.fromString "contentMarkdown"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    disabled <- v Data.Aeson..: Data.Aeson.Key.fromString "disabled"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    lang <- v Data.Aeson..:? Data.Aeson.Key.fromString "lang"
    lastUpdatedBy <- v Data.Aeson..: Data.Aeson.Key.fromString "lastUpdatedBy"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    subject <- v Data.Aeson..:? Data.Aeson.Key.fromString "subject"
    updatedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "updatedAt"
    pure $ TemplateView contentMarkdown createdAt disabled id' lang lastUpdatedBy name subject updatedAt

instance Data.Aeson.ToJSON TemplateView where
  toJSON (TemplateView contentMarkdown createdAt disabled id' lang lastUpdatedBy name subject updatedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "contentMarkdown" Data.Aeson..= contentMarkdown
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lang" Data.Aeson..?= lang
        , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..= lastUpdatedBy
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "subject" Data.Aeson..?= subject
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]
  toEncoding (TemplateView contentMarkdown createdAt disabled id' lang lastUpdatedBy name subject updatedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "contentMarkdown" Data.Aeson..= contentMarkdown
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lang" Data.Aeson..?= lang
        , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..= lastUpdatedBy
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "subject" Data.Aeson..?= subject
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]

templateView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => TemplateView -> kv
templateView'AesonFields (TemplateView contentMarkdown createdAt disabled id' lang lastUpdatedBy name subject updatedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "contentMarkdown" Data.Aeson..= contentMarkdown
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "disabled" Data.Aeson..= disabled
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "lang" Data.Aeson..?= lang
    , Data.Aeson.Key.fromString "lastUpdatedBy" Data.Aeson..= lastUpdatedBy
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "subject" Data.Aeson..?= subject
    , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
    ]
