{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.QueryEvents where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

data QueryEventsResult = QueryEventsResult
  { cursor :: Maybe Data.Text.Text
  , events :: [Tools.Ozone.Moderation.Defs.ModEventView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON QueryEventsResult where
  parseJSON = Data.Aeson.withObject "QueryEventsResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    events <- v Data.Aeson..: Data.Aeson.Key.fromString "events"
    pure $ QueryEventsResult cursor events

instance Data.Aeson.ToJSON QueryEventsResult where
  toJSON (QueryEventsResult cursor events) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "events" Data.Aeson..= events
        ]
  toEncoding (QueryEventsResult cursor events) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "events" Data.Aeson..= events
        ]

queryEventsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryEventsResult -> kv
queryEventsResult'AesonFields (QueryEventsResult cursor events) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "events" Data.Aeson..= events
    ]

type QueryEvents = "tools.ozone.moderation.queryEvents" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "addedLabels" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "addedTags" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "comment" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "createdAfter" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "createdBefore" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "createdBy" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "hasComment" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "includeAllUserRecords" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "removedLabels" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "removedTags" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reportTypes" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sortDirection" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "subject" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "types" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] QueryEventsResult
