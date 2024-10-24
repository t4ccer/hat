{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.QueryStatuses where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

data QueryStatusesResult = QueryStatusesResult
  { cursor :: Maybe Data.Text.Text
  , subjectStatuses :: [Tools.Ozone.Moderation.Defs.SubjectStatusView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON QueryStatusesResult where
  parseJSON = Data.Aeson.withObject "QueryStatusesResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    subjectStatuses <- v Data.Aeson..: Data.Aeson.Key.fromString "subjectStatuses"
    pure $ QueryStatusesResult cursor subjectStatuses

instance Data.Aeson.ToJSON QueryStatusesResult where
  toJSON (QueryStatusesResult cursor subjectStatuses) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "subjectStatuses" Data.Aeson..= subjectStatuses
        ]
  toEncoding (QueryStatusesResult cursor subjectStatuses) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "subjectStatuses" Data.Aeson..= subjectStatuses
        ]

queryStatusesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => QueryStatusesResult -> kv
queryStatusesResult'AesonFields (QueryStatusesResult cursor subjectStatuses) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "subjectStatuses" Data.Aeson..= subjectStatuses
    ]

type QueryStatuses = "tools.ozone.moderation.queryStatuses" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "appealed" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "comment" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "excludeTags" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "ignoreSubjects" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "includeAllUserRecords" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "includeMuted" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "lastReviewedBy" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "onlyMuted" Bool Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reportedAfter" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reportedBefore" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reviewState" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reviewedAfter" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "reviewedBefore" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sortDirection" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "sortField" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "subject" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "tags" [Data.Text.Text] Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "takendown" Bool Servant.API.:> Servant.API.Get '[Servant.API.JSON] QueryStatusesResult
