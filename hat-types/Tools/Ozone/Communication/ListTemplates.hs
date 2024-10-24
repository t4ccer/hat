{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Communication.ListTemplates where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Communication.Defs

data ListTemplatesResult = ListTemplatesResult
  { communicationTemplates :: [Tools.Ozone.Communication.Defs.TemplateView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListTemplatesResult where
  parseJSON = Data.Aeson.withObject "ListTemplatesResult" $ \v -> do
    communicationTemplates <- v Data.Aeson..: Data.Aeson.Key.fromString "communicationTemplates"
    pure $ ListTemplatesResult communicationTemplates

instance Data.Aeson.ToJSON ListTemplatesResult where
  toJSON (ListTemplatesResult communicationTemplates) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "communicationTemplates" Data.Aeson..= communicationTemplates
        ]
  toEncoding (ListTemplatesResult communicationTemplates) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "communicationTemplates" Data.Aeson..= communicationTemplates
        ]

listTemplatesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListTemplatesResult -> kv
listTemplatesResult'AesonFields (ListTemplatesResult communicationTemplates) =
  mconcat
    [ Data.Aeson.Key.fromString "communicationTemplates" Data.Aeson..= communicationTemplates
    ]

type ListTemplates = "tools.ozone.communication.listTemplates" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListTemplatesResult
