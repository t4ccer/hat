{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Set.QuerySets where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Set.Defs

data QuerySetsResult = QuerySetsResult
  { cursor :: Maybe Data.Text.Text
  , sets :: [Tools.Ozone.Set.Defs.SetView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON QuerySetsResult where
  parseJSON = Data.Aeson.withObject "QuerySetsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    sets <- v Data.Aeson..: Data.Aeson.Key.fromString "sets"
    pure $ QuerySetsResult cursor sets

instance Data.Aeson.ToJSON QuerySetsResult where
  toJSON (QuerySetsResult cursor sets) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "sets" Data.Aeson..= sets
        ]
  toEncoding (QuerySetsResult cursor sets) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "sets" Data.Aeson..= sets
        ]

querySetsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QuerySetsResult -> kv
querySetsResult'AesonFields (QuerySetsResult cursor sets) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "sets" Data.Aeson..= sets
    ]

type QuerySets = "tools.ozone.set.querySets" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "namePrefix" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sortBy" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sortDirection" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] QuerySetsResult
