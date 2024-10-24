{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.GetTaggedSuggestions where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetTaggedSuggestionsResult = GetTaggedSuggestionsResult
  { suggestions :: [Suggestion]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetTaggedSuggestionsResult where
  parseJSON = Data.Aeson.withObject "GetTaggedSuggestionsResult" $ \v -> do
    suggestions <- v Data.Aeson..: Data.Aeson.Key.fromString "suggestions"
    pure $ GetTaggedSuggestionsResult suggestions

instance Data.Aeson.ToJSON GetTaggedSuggestionsResult where
  toJSON (GetTaggedSuggestionsResult suggestions) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
        ]
  toEncoding (GetTaggedSuggestionsResult suggestions) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
        ]

getTaggedSuggestionsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetTaggedSuggestionsResult -> kv
getTaggedSuggestionsResult'AesonFields (GetTaggedSuggestionsResult suggestions) =
  mconcat
    [ Data.Aeson.Key.fromString "suggestions" Data.Aeson..= suggestions
    ]

type GetTaggedSuggestions = "app.bsky.unspecced.getTaggedSuggestions" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetTaggedSuggestionsResult
data Suggestion = Suggestion
  { subject :: Data.Text.Text
  , subjectType :: Data.Text.Text
  , tag :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Suggestion where
  parseJSON = Data.Aeson.withObject "Suggestion" $ \v -> do
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    subjectType <- v Data.Aeson..: Data.Aeson.Key.fromString "subjectType"
    tag <- v Data.Aeson..: Data.Aeson.Key.fromString "tag"
    pure $ Suggestion subject subjectType tag

instance Data.Aeson.ToJSON Suggestion where
  toJSON (Suggestion subject subjectType tag) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectType" Data.Aeson..= subjectType
        , Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
        ]
  toEncoding (Suggestion subject subjectType tag) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectType" Data.Aeson..= subjectType
        , Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
        ]

suggestion'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Suggestion -> kv
suggestion'AesonFields (Suggestion subject subjectType tag) =
  mconcat
    [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "subjectType" Data.Aeson..= subjectType
    , Data.Aeson.Key.fromString "tag" Data.Aeson..= tag
    ]
