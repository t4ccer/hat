{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Set.GetValues where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Set.Defs

data GetValuesResult = GetValuesResult
  { cursor :: Maybe Data.Text.Text
  , set :: Tools.Ozone.Set.Defs.SetView
  , values :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetValuesResult where
  parseJSON = Data.Aeson.withObject "GetValuesResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    set <- v Data.Aeson..: Data.Aeson.Key.fromString "set"
    values <- v Data.Aeson..: Data.Aeson.Key.fromString "values"
    pure $ GetValuesResult cursor set values

instance Data.Aeson.ToJSON GetValuesResult where
  toJSON (GetValuesResult cursor set values) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "set" Data.Aeson..= set
        , Data.Aeson.Key.fromString "values" Data.Aeson..= values
        ]
  toEncoding (GetValuesResult cursor set values) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "set" Data.Aeson..= set
        , Data.Aeson.Key.fromString "values" Data.Aeson..= values
        ]

getValuesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetValuesResult -> kv
getValuesResult'AesonFields (GetValuesResult cursor set values) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "set" Data.Aeson..= set
    , Data.Aeson.Key.fromString "values" Data.Aeson..= values
    ]

type GetValues = "tools.ozone.set.getValues" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "name" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetValuesResult
