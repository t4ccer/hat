{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.Defs where

import {-# SOURCE #-} qualified Chat.Bsky.Convo.Defs
import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import {-# SOURCE #-} qualified Com.Atproto.Moderation.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import {-# SOURCE #-} qualified Com.Atproto.Server.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data BlobView = BlobView
  { cid :: Data.Text.Text
  , createdAt :: Data.Text.Text
  , details :: Maybe BlobViewDetailsKind
  , mimeType :: Data.Text.Text
  , moderation :: Maybe Moderation
  , size :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BlobView where
  parseJSON = Data.Aeson.withObject "BlobView" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    details <- v Data.Aeson..:? Data.Aeson.Key.fromString "details"
    mimeType <- v Data.Aeson..: Data.Aeson.Key.fromString "mimeType"
    moderation <- v Data.Aeson..:? Data.Aeson.Key.fromString "moderation"
    size <- v Data.Aeson..: Data.Aeson.Key.fromString "size"
    pure $ BlobView cid createdAt details mimeType moderation size

instance Data.Aeson.ToJSON BlobView where
  toJSON (BlobView cid createdAt details mimeType moderation size) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "details" Data.Aeson..?= details
        , Data.Aeson.Key.fromString "mimeType" Data.Aeson..= mimeType
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..?= moderation
        , Data.Aeson.Key.fromString "size" Data.Aeson..= size
        ]
  toEncoding (BlobView cid createdAt details mimeType moderation size) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "details" Data.Aeson..?= details
        , Data.Aeson.Key.fromString "mimeType" Data.Aeson..= mimeType
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..?= moderation
        , Data.Aeson.Key.fromString "size" Data.Aeson..= size
        ]

blobView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlobView -> kv
blobView'AesonFields (BlobView cid createdAt details mimeType moderation size) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "details" Data.Aeson..?= details
    , Data.Aeson.Key.fromString "mimeType" Data.Aeson..= mimeType
    , Data.Aeson.Key.fromString "moderation" Data.Aeson..?= moderation
    , Data.Aeson.Key.fromString "size" Data.Aeson..= size
    ]

data ImageDetails = ImageDetails
  { height :: Integer
  , width :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ImageDetails where
  parseJSON = Data.Aeson.withObject "ImageDetails" $ \v -> do
    height <- v Data.Aeson..: Data.Aeson.Key.fromString "height"
    width <- v Data.Aeson..: Data.Aeson.Key.fromString "width"
    pure $ ImageDetails height width

instance Data.Aeson.ToJSON ImageDetails where
  toJSON (ImageDetails height width) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]
  toEncoding (ImageDetails height width) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]

imageDetails'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ImageDetails -> kv
imageDetails'AesonFields (ImageDetails height width) =
  mconcat
    [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
    , Data.Aeson.Key.fromString "width" Data.Aeson..= width
    ]

data ModEventAcknowledge = ModEventAcknowledge
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventAcknowledge where
  parseJSON = Data.Aeson.withObject "ModEventAcknowledge" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventAcknowledge comment

instance Data.Aeson.ToJSON ModEventAcknowledge where
  toJSON (ModEventAcknowledge comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventAcknowledge comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventAcknowledge'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventAcknowledge -> kv
modEventAcknowledge'AesonFields (ModEventAcknowledge comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventComment = ModEventComment
  { comment :: Data.Text.Text
  , sticky :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventComment where
  parseJSON = Data.Aeson.withObject "ModEventComment" $ \v -> do
    comment <- v Data.Aeson..: Data.Aeson.Key.fromString "comment"
    sticky <- v Data.Aeson..:? Data.Aeson.Key.fromString "sticky"
    pure $ ModEventComment comment sticky

instance Data.Aeson.ToJSON ModEventComment where
  toJSON (ModEventComment comment sticky) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..= comment
        , Data.Aeson.Key.fromString "sticky" Data.Aeson..?= sticky
        ]
  toEncoding (ModEventComment comment sticky) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..= comment
        , Data.Aeson.Key.fromString "sticky" Data.Aeson..?= sticky
        ]

modEventComment'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventComment -> kv
modEventComment'AesonFields (ModEventComment comment sticky) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..= comment
    , Data.Aeson.Key.fromString "sticky" Data.Aeson..?= sticky
    ]

data ModEventDivert = ModEventDivert
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventDivert where
  parseJSON = Data.Aeson.withObject "ModEventDivert" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventDivert comment

instance Data.Aeson.ToJSON ModEventDivert where
  toJSON (ModEventDivert comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventDivert comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventDivert'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventDivert -> kv
modEventDivert'AesonFields (ModEventDivert comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventEmail = ModEventEmail
  { comment :: Maybe Data.Text.Text
  , content :: Maybe Data.Text.Text
  , subjectLine :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventEmail where
  parseJSON = Data.Aeson.withObject "ModEventEmail" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    content <- v Data.Aeson..:? Data.Aeson.Key.fromString "content"
    subjectLine <- v Data.Aeson..: Data.Aeson.Key.fromString "subjectLine"
    pure $ ModEventEmail comment content subjectLine

instance Data.Aeson.ToJSON ModEventEmail where
  toJSON (ModEventEmail comment content subjectLine) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "content" Data.Aeson..?= content
        , Data.Aeson.Key.fromString "subjectLine" Data.Aeson..= subjectLine
        ]
  toEncoding (ModEventEmail comment content subjectLine) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "content" Data.Aeson..?= content
        , Data.Aeson.Key.fromString "subjectLine" Data.Aeson..= subjectLine
        ]

modEventEmail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventEmail -> kv
modEventEmail'AesonFields (ModEventEmail comment content subjectLine) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "content" Data.Aeson..?= content
    , Data.Aeson.Key.fromString "subjectLine" Data.Aeson..= subjectLine
    ]

data ModEventEscalate = ModEventEscalate
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventEscalate where
  parseJSON = Data.Aeson.withObject "ModEventEscalate" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventEscalate comment

instance Data.Aeson.ToJSON ModEventEscalate where
  toJSON (ModEventEscalate comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventEscalate comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventEscalate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventEscalate -> kv
modEventEscalate'AesonFields (ModEventEscalate comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventLabel = ModEventLabel
  { comment :: Maybe Data.Text.Text
  , createLabelVals :: [Data.Text.Text]
  , negateLabelVals :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventLabel where
  parseJSON = Data.Aeson.withObject "ModEventLabel" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    createLabelVals <- v Data.Aeson..: Data.Aeson.Key.fromString "createLabelVals"
    negateLabelVals <- v Data.Aeson..: Data.Aeson.Key.fromString "negateLabelVals"
    pure $ ModEventLabel comment createLabelVals negateLabelVals

instance Data.Aeson.ToJSON ModEventLabel where
  toJSON (ModEventLabel comment createLabelVals negateLabelVals) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "createLabelVals" Data.Aeson..= createLabelVals
        , Data.Aeson.Key.fromString "negateLabelVals" Data.Aeson..= negateLabelVals
        ]
  toEncoding (ModEventLabel comment createLabelVals negateLabelVals) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "createLabelVals" Data.Aeson..= createLabelVals
        , Data.Aeson.Key.fromString "negateLabelVals" Data.Aeson..= negateLabelVals
        ]

modEventLabel'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventLabel -> kv
modEventLabel'AesonFields (ModEventLabel comment createLabelVals negateLabelVals) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "createLabelVals" Data.Aeson..= createLabelVals
    , Data.Aeson.Key.fromString "negateLabelVals" Data.Aeson..= negateLabelVals
    ]

data ModEventMute = ModEventMute
  { comment :: Maybe Data.Text.Text
  , durationInHours :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventMute where
  parseJSON = Data.Aeson.withObject "ModEventMute" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    durationInHours <- v Data.Aeson..: Data.Aeson.Key.fromString "durationInHours"
    pure $ ModEventMute comment durationInHours

instance Data.Aeson.ToJSON ModEventMute where
  toJSON (ModEventMute comment durationInHours) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
        ]
  toEncoding (ModEventMute comment durationInHours) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
        ]

modEventMute'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventMute -> kv
modEventMute'AesonFields (ModEventMute comment durationInHours) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
    ]

data ModEventMuteReporter = ModEventMuteReporter
  { comment :: Maybe Data.Text.Text
  , durationInHours :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventMuteReporter where
  parseJSON = Data.Aeson.withObject "ModEventMuteReporter" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    durationInHours <- v Data.Aeson..: Data.Aeson.Key.fromString "durationInHours"
    pure $ ModEventMuteReporter comment durationInHours

instance Data.Aeson.ToJSON ModEventMuteReporter where
  toJSON (ModEventMuteReporter comment durationInHours) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
        ]
  toEncoding (ModEventMuteReporter comment durationInHours) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
        ]

modEventMuteReporter'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventMuteReporter -> kv
modEventMuteReporter'AesonFields (ModEventMuteReporter comment durationInHours) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..= durationInHours
    ]

data ModEventReport = ModEventReport
  { comment :: Maybe Data.Text.Text
  , isReporterMuted :: Maybe Bool
  , reportType :: Com.Atproto.Moderation.Defs.ReasonType
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventReport where
  parseJSON = Data.Aeson.withObject "ModEventReport" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    isReporterMuted <- v Data.Aeson..:? Data.Aeson.Key.fromString "isReporterMuted"
    reportType <- v Data.Aeson..: Data.Aeson.Key.fromString "reportType"
    pure $ ModEventReport comment isReporterMuted reportType

instance Data.Aeson.ToJSON ModEventReport where
  toJSON (ModEventReport comment isReporterMuted reportType) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "isReporterMuted" Data.Aeson..?= isReporterMuted
        , Data.Aeson.Key.fromString "reportType" Data.Aeson..= reportType
        ]
  toEncoding (ModEventReport comment isReporterMuted reportType) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "isReporterMuted" Data.Aeson..?= isReporterMuted
        , Data.Aeson.Key.fromString "reportType" Data.Aeson..= reportType
        ]

modEventReport'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventReport -> kv
modEventReport'AesonFields (ModEventReport comment isReporterMuted reportType) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "isReporterMuted" Data.Aeson..?= isReporterMuted
    , Data.Aeson.Key.fromString "reportType" Data.Aeson..= reportType
    ]

data ModEventResolveAppeal = ModEventResolveAppeal
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventResolveAppeal where
  parseJSON = Data.Aeson.withObject "ModEventResolveAppeal" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventResolveAppeal comment

instance Data.Aeson.ToJSON ModEventResolveAppeal where
  toJSON (ModEventResolveAppeal comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventResolveAppeal comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventResolveAppeal'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventResolveAppeal -> kv
modEventResolveAppeal'AesonFields (ModEventResolveAppeal comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventReverseTakedown = ModEventReverseTakedown
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventReverseTakedown where
  parseJSON = Data.Aeson.withObject "ModEventReverseTakedown" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventReverseTakedown comment

instance Data.Aeson.ToJSON ModEventReverseTakedown where
  toJSON (ModEventReverseTakedown comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventReverseTakedown comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventReverseTakedown'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventReverseTakedown -> kv
modEventReverseTakedown'AesonFields (ModEventReverseTakedown comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventTag = ModEventTag
  { add :: [Data.Text.Text]
  , comment :: Maybe Data.Text.Text
  , remove :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventTag where
  parseJSON = Data.Aeson.withObject "ModEventTag" $ \v -> do
    add <- v Data.Aeson..: Data.Aeson.Key.fromString "add"
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    remove <- v Data.Aeson..: Data.Aeson.Key.fromString "remove"
    pure $ ModEventTag add comment remove

instance Data.Aeson.ToJSON ModEventTag where
  toJSON (ModEventTag add comment remove) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "add" Data.Aeson..= add
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "remove" Data.Aeson..= remove
        ]
  toEncoding (ModEventTag add comment remove) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "add" Data.Aeson..= add
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "remove" Data.Aeson..= remove
        ]

modEventTag'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventTag -> kv
modEventTag'AesonFields (ModEventTag add comment remove) =
  mconcat
    [ Data.Aeson.Key.fromString "add" Data.Aeson..= add
    , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "remove" Data.Aeson..= remove
    ]

data ModEventTakedown = ModEventTakedown
  { acknowledgeAccountSubjects :: Maybe Bool
  , comment :: Maybe Data.Text.Text
  , durationInHours :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventTakedown where
  parseJSON = Data.Aeson.withObject "ModEventTakedown" $ \v -> do
    acknowledgeAccountSubjects <- v Data.Aeson..:? Data.Aeson.Key.fromString "acknowledgeAccountSubjects"
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    durationInHours <- v Data.Aeson..:? Data.Aeson.Key.fromString "durationInHours"
    pure $ ModEventTakedown acknowledgeAccountSubjects comment durationInHours

instance Data.Aeson.ToJSON ModEventTakedown where
  toJSON (ModEventTakedown acknowledgeAccountSubjects comment durationInHours) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "acknowledgeAccountSubjects" Data.Aeson..?= acknowledgeAccountSubjects
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..?= durationInHours
        ]
  toEncoding (ModEventTakedown acknowledgeAccountSubjects comment durationInHours) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "acknowledgeAccountSubjects" Data.Aeson..?= acknowledgeAccountSubjects
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..?= durationInHours
        ]

modEventTakedown'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventTakedown -> kv
modEventTakedown'AesonFields (ModEventTakedown acknowledgeAccountSubjects comment durationInHours) =
  mconcat
    [ Data.Aeson.Key.fromString "acknowledgeAccountSubjects" Data.Aeson..?= acknowledgeAccountSubjects
    , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "durationInHours" Data.Aeson..?= durationInHours
    ]

data ModEventUnmute = ModEventUnmute
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventUnmute where
  parseJSON = Data.Aeson.withObject "ModEventUnmute" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventUnmute comment

instance Data.Aeson.ToJSON ModEventUnmute where
  toJSON (ModEventUnmute comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventUnmute comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventUnmute'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventUnmute -> kv
modEventUnmute'AesonFields (ModEventUnmute comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventUnmuteReporter = ModEventUnmuteReporter
  { comment :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventUnmuteReporter where
  parseJSON = Data.Aeson.withObject "ModEventUnmuteReporter" $ \v -> do
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    pure $ ModEventUnmuteReporter comment

instance Data.Aeson.ToJSON ModEventUnmuteReporter where
  toJSON (ModEventUnmuteReporter comment) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]
  toEncoding (ModEventUnmuteReporter comment) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        ]

modEventUnmuteReporter'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventUnmuteReporter -> kv
modEventUnmuteReporter'AesonFields (ModEventUnmuteReporter comment) =
  mconcat
    [ Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    ]

data ModEventView = ModEventView
  { createdAt :: Data.Text.Text
  , createdBy :: Data.Text.Text
  , creatorHandle :: Maybe Data.Text.Text
  , event :: ModEventViewEventKind
  , id' :: Integer
  , subject :: ModEventViewSubjectKind
  , subjectBlobCids :: [Data.Text.Text]
  , subjectHandle :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventView where
  parseJSON = Data.Aeson.withObject "ModEventView" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    createdBy <- v Data.Aeson..: Data.Aeson.Key.fromString "createdBy"
    creatorHandle <- v Data.Aeson..:? Data.Aeson.Key.fromString "creatorHandle"
    event <- v Data.Aeson..: Data.Aeson.Key.fromString "event"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    subjectBlobCids <- v Data.Aeson..: Data.Aeson.Key.fromString "subjectBlobCids"
    subjectHandle <- v Data.Aeson..:? Data.Aeson.Key.fromString "subjectHandle"
    pure $ ModEventView createdAt createdBy creatorHandle event id' subject subjectBlobCids subjectHandle

instance Data.Aeson.ToJSON ModEventView where
  toJSON (ModEventView createdAt createdBy creatorHandle event id' subject subjectBlobCids subjectHandle) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "creatorHandle" Data.Aeson..?= creatorHandle
        , Data.Aeson.Key.fromString "event" Data.Aeson..= event
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..= subjectBlobCids
        , Data.Aeson.Key.fromString "subjectHandle" Data.Aeson..?= subjectHandle
        ]
  toEncoding (ModEventView createdAt createdBy creatorHandle event id' subject subjectBlobCids subjectHandle) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "creatorHandle" Data.Aeson..?= creatorHandle
        , Data.Aeson.Key.fromString "event" Data.Aeson..= event
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..= subjectBlobCids
        , Data.Aeson.Key.fromString "subjectHandle" Data.Aeson..?= subjectHandle
        ]

modEventView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventView -> kv
modEventView'AesonFields (ModEventView createdAt createdBy creatorHandle event id' subject subjectBlobCids subjectHandle) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
    , Data.Aeson.Key.fromString "creatorHandle" Data.Aeson..?= creatorHandle
    , Data.Aeson.Key.fromString "event" Data.Aeson..= event
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..= subjectBlobCids
    , Data.Aeson.Key.fromString "subjectHandle" Data.Aeson..?= subjectHandle
    ]

data ModEventViewDetail = ModEventViewDetail
  { createdAt :: Data.Text.Text
  , createdBy :: Data.Text.Text
  , event :: ModEventViewDetailEventKind
  , id' :: Integer
  , subject :: ModEventViewDetailSubjectKind
  , subjectBlobs :: [BlobView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventViewDetail where
  parseJSON = Data.Aeson.withObject "ModEventViewDetail" $ \v -> do
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    createdBy <- v Data.Aeson..: Data.Aeson.Key.fromString "createdBy"
    event <- v Data.Aeson..: Data.Aeson.Key.fromString "event"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    subjectBlobs <- v Data.Aeson..: Data.Aeson.Key.fromString "subjectBlobs"
    pure $ ModEventViewDetail createdAt createdBy event id' subject subjectBlobs

instance Data.Aeson.ToJSON ModEventViewDetail where
  toJSON (ModEventViewDetail createdAt createdBy event id' subject subjectBlobs) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "event" Data.Aeson..= event
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobs" Data.Aeson..= subjectBlobs
        ]
  toEncoding (ModEventViewDetail createdAt createdBy event id' subject subjectBlobs) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
        , Data.Aeson.Key.fromString "event" Data.Aeson..= event
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobs" Data.Aeson..= subjectBlobs
        ]

modEventViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventViewDetail -> kv
modEventViewDetail'AesonFields (ModEventViewDetail createdAt createdBy event id' subject subjectBlobs) =
  mconcat
    [ Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "createdBy" Data.Aeson..= createdBy
    , Data.Aeson.Key.fromString "event" Data.Aeson..= event
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "subjectBlobs" Data.Aeson..= subjectBlobs
    ]

data Moderation = Moderation
  { subjectStatus :: Maybe SubjectStatusView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Moderation where
  parseJSON = Data.Aeson.withObject "Moderation" $ \v -> do
    subjectStatus <- v Data.Aeson..:? Data.Aeson.Key.fromString "subjectStatus"
    pure $ Moderation subjectStatus

instance Data.Aeson.ToJSON Moderation where
  toJSON (Moderation subjectStatus) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
        ]
  toEncoding (Moderation subjectStatus) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
        ]

moderation'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Moderation -> kv
moderation'AesonFields (Moderation subjectStatus) =
  mconcat
    [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
    ]

data ModerationDetail = ModerationDetail
  { subjectStatus :: Maybe SubjectStatusView
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModerationDetail where
  parseJSON = Data.Aeson.withObject "ModerationDetail" $ \v -> do
    subjectStatus <- v Data.Aeson..:? Data.Aeson.Key.fromString "subjectStatus"
    pure $ ModerationDetail subjectStatus

instance Data.Aeson.ToJSON ModerationDetail where
  toJSON (ModerationDetail subjectStatus) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
        ]
  toEncoding (ModerationDetail subjectStatus) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
        ]

moderationDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModerationDetail -> kv
moderationDetail'AesonFields (ModerationDetail subjectStatus) =
  mconcat
    [ Data.Aeson.Key.fromString "subjectStatus" Data.Aeson..?= subjectStatus
    ]

data RecordView = RecordView
  { blobCids :: [Data.Text.Text]
  , cid :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , moderation :: Moderation
  , repo :: RepoView
  , uri :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordView where
  parseJSON = Data.Aeson.withObject "RecordView" $ \v -> do
    blobCids <- v Data.Aeson..: Data.Aeson.Key.fromString "blobCids"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    moderation <- v Data.Aeson..: Data.Aeson.Key.fromString "moderation"
    repo <- v Data.Aeson..: Data.Aeson.Key.fromString "repo"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ RecordView blobCids cid indexedAt moderation repo uri value

instance Data.Aeson.ToJSON RecordView where
  toJSON (RecordView blobCids cid indexedAt moderation repo uri value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blobCids" Data.Aeson..= blobCids
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (RecordView blobCids cid indexedAt moderation repo uri value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blobCids" Data.Aeson..= blobCids
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

recordView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordView -> kv
recordView'AesonFields (RecordView blobCids cid indexedAt moderation repo uri value) =
  mconcat
    [ Data.Aeson.Key.fromString "blobCids" Data.Aeson..= blobCids
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
    , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data RecordViewDetail = RecordViewDetail
  { blobs :: [BlobView]
  , cid :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , moderation :: ModerationDetail
  , repo :: RepoView
  , uri :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordViewDetail where
  parseJSON = Data.Aeson.withObject "RecordViewDetail" $ \v -> do
    blobs <- v Data.Aeson..: Data.Aeson.Key.fromString "blobs"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    moderation <- v Data.Aeson..: Data.Aeson.Key.fromString "moderation"
    repo <- v Data.Aeson..: Data.Aeson.Key.fromString "repo"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ RecordViewDetail blobs cid indexedAt labels moderation repo uri value

instance Data.Aeson.ToJSON RecordViewDetail where
  toJSON (RecordViewDetail blobs cid indexedAt labels moderation repo uri value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (RecordViewDetail blobs cid indexedAt labels moderation repo uri value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

recordViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordViewDetail -> kv
recordViewDetail'AesonFields (RecordViewDetail blobs cid indexedAt labels moderation repo uri value) =
  mconcat
    [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
    , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data RecordViewNotFound = RecordViewNotFound
  { uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RecordViewNotFound where
  parseJSON = Data.Aeson.withObject "RecordViewNotFound" $ \v -> do
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ RecordViewNotFound uri

instance Data.Aeson.ToJSON RecordViewNotFound where
  toJSON (RecordViewNotFound uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (RecordViewNotFound uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

recordViewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordViewNotFound -> kv
recordViewNotFound'AesonFields (RecordViewNotFound uri) =
  mconcat
    [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data RepoView = RepoView
  { deactivatedAt :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , email :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , inviteNote :: Maybe Data.Text.Text
  , invitedBy :: Maybe Com.Atproto.Server.Defs.InviteCode
  , invitesDisabled :: Maybe Bool
  , moderation :: Moderation
  , relatedRecords :: [Data.Aeson.Value]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoView where
  parseJSON = Data.Aeson.withObject "RepoView" $ \v -> do
    deactivatedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "deactivatedAt"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    email <- v Data.Aeson..:? Data.Aeson.Key.fromString "email"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    inviteNote <- v Data.Aeson..:? Data.Aeson.Key.fromString "inviteNote"
    invitedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitedBy"
    invitesDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitesDisabled"
    moderation <- v Data.Aeson..: Data.Aeson.Key.fromString "moderation"
    relatedRecords <- v Data.Aeson..: Data.Aeson.Key.fromString "relatedRecords"
    pure $ RepoView deactivatedAt did email handle indexedAt inviteNote invitedBy invitesDisabled moderation relatedRecords

instance Data.Aeson.ToJSON RepoView where
  toJSON (RepoView deactivatedAt did email handle indexedAt inviteNote invitedBy invitesDisabled moderation relatedRecords) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
        ]
  toEncoding (RepoView deactivatedAt did email handle indexedAt inviteNote invitedBy invitesDisabled moderation relatedRecords) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
        ]

repoView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoView -> kv
repoView'AesonFields (RepoView deactivatedAt did email handle indexedAt inviteNote invitedBy invitesDisabled moderation relatedRecords) =
  mconcat
    [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
    , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
    , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
    , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
    , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
    ]

data RepoViewDetail = RepoViewDetail
  { deactivatedAt :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , email :: Maybe Data.Text.Text
  , emailConfirmedAt :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , inviteNote :: Maybe Data.Text.Text
  , invitedBy :: Maybe Com.Atproto.Server.Defs.InviteCode
  , invites :: Maybe [Com.Atproto.Server.Defs.InviteCode]
  , invitesDisabled :: Maybe Bool
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , moderation :: ModerationDetail
  , relatedRecords :: [Data.Aeson.Value]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoViewDetail where
  parseJSON = Data.Aeson.withObject "RepoViewDetail" $ \v -> do
    deactivatedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "deactivatedAt"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    email <- v Data.Aeson..:? Data.Aeson.Key.fromString "email"
    emailConfirmedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "emailConfirmedAt"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    inviteNote <- v Data.Aeson..:? Data.Aeson.Key.fromString "inviteNote"
    invitedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitedBy"
    invites <- v Data.Aeson..:? Data.Aeson.Key.fromString "invites"
    invitesDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "invitesDisabled"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    moderation <- v Data.Aeson..: Data.Aeson.Key.fromString "moderation"
    relatedRecords <- v Data.Aeson..: Data.Aeson.Key.fromString "relatedRecords"
    pure $ RepoViewDetail deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled labels moderation relatedRecords

instance Data.Aeson.ToJSON RepoViewDetail where
  toJSON (RepoViewDetail deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled labels moderation relatedRecords) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
        ]
  toEncoding (RepoViewDetail deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled labels moderation relatedRecords) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
        , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
        , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
        , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
        , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
        , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
        ]

repoViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoViewDetail -> kv
repoViewDetail'AesonFields (RepoViewDetail deactivatedAt did email emailConfirmedAt handle indexedAt inviteNote invitedBy invites invitesDisabled labels moderation relatedRecords) =
  mconcat
    [ Data.Aeson.Key.fromString "deactivatedAt" Data.Aeson..?= deactivatedAt
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "email" Data.Aeson..?= email
    , Data.Aeson.Key.fromString "emailConfirmedAt" Data.Aeson..?= emailConfirmedAt
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "inviteNote" Data.Aeson..?= inviteNote
    , Data.Aeson.Key.fromString "invitedBy" Data.Aeson..?= invitedBy
    , Data.Aeson.Key.fromString "invites" Data.Aeson..?= invites
    , Data.Aeson.Key.fromString "invitesDisabled" Data.Aeson..?= invitesDisabled
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "moderation" Data.Aeson..= moderation
    , Data.Aeson.Key.fromString "relatedRecords" Data.Aeson..= relatedRecords
    ]

data RepoViewNotFound = RepoViewNotFound
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoViewNotFound where
  parseJSON = Data.Aeson.withObject "RepoViewNotFound" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ RepoViewNotFound did

instance Data.Aeson.ToJSON RepoViewNotFound where
  toJSON (RepoViewNotFound did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (RepoViewNotFound did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

repoViewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoViewNotFound -> kv
repoViewNotFound'AesonFields (RepoViewNotFound did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

newtype SubjectReviewState = SubjectReviewState
  { getSubjectReviewState :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SubjectReviewState where
  parseJSON = Data.Aeson.withText "SubjectReviewState" $ pure . SubjectReviewState

instance Data.Aeson.ToJSON SubjectReviewState where
  toJSON (SubjectReviewState getSubjectReviewState) = Data.Aeson.toJSON getSubjectReviewState
  toEncoding (SubjectReviewState getSubjectReviewState) = Data.Aeson.toEncoding getSubjectReviewState

data SubjectStatusView = SubjectStatusView
  { appealed :: Maybe Bool
  , comment :: Maybe Data.Text.Text
  , createdAt :: Data.Text.Text
  , id' :: Integer
  , lastAppealedAt :: Maybe Data.Text.Text
  , lastReportedAt :: Maybe Data.Text.Text
  , lastReviewedAt :: Maybe Data.Text.Text
  , lastReviewedBy :: Maybe Data.Text.Text
  , muteReportingUntil :: Maybe Data.Text.Text
  , muteUntil :: Maybe Data.Text.Text
  , reviewState :: SubjectReviewState
  , subject :: SubjectStatusViewSubjectKind
  , subjectBlobCids :: Maybe [Data.Text.Text]
  , subjectRepoHandle :: Maybe Data.Text.Text
  , suspendUntil :: Maybe Data.Text.Text
  , tags :: Maybe [Data.Text.Text]
  , takendown :: Maybe Bool
  , updatedAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SubjectStatusView where
  parseJSON = Data.Aeson.withObject "SubjectStatusView" $ \v -> do
    appealed <- v Data.Aeson..:? Data.Aeson.Key.fromString "appealed"
    comment <- v Data.Aeson..:? Data.Aeson.Key.fromString "comment"
    createdAt <- v Data.Aeson..: Data.Aeson.Key.fromString "createdAt"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    lastAppealedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastAppealedAt"
    lastReportedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastReportedAt"
    lastReviewedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastReviewedAt"
    lastReviewedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "lastReviewedBy"
    muteReportingUntil <- v Data.Aeson..:? Data.Aeson.Key.fromString "muteReportingUntil"
    muteUntil <- v Data.Aeson..:? Data.Aeson.Key.fromString "muteUntil"
    reviewState <- v Data.Aeson..: Data.Aeson.Key.fromString "reviewState"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    subjectBlobCids <- v Data.Aeson..:? Data.Aeson.Key.fromString "subjectBlobCids"
    subjectRepoHandle <- v Data.Aeson..:? Data.Aeson.Key.fromString "subjectRepoHandle"
    suspendUntil <- v Data.Aeson..:? Data.Aeson.Key.fromString "suspendUntil"
    tags <- v Data.Aeson..:? Data.Aeson.Key.fromString "tags"
    takendown <- v Data.Aeson..:? Data.Aeson.Key.fromString "takendown"
    updatedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "updatedAt"
    pure $ SubjectStatusView appealed comment createdAt id' lastAppealedAt lastReportedAt lastReviewedAt lastReviewedBy muteReportingUntil muteUntil reviewState subject subjectBlobCids subjectRepoHandle suspendUntil tags takendown updatedAt

instance Data.Aeson.ToJSON SubjectStatusView where
  toJSON (SubjectStatusView appealed comment createdAt id' lastAppealedAt lastReportedAt lastReviewedAt lastReviewedBy muteReportingUntil muteUntil reviewState subject subjectBlobCids subjectRepoHandle suspendUntil tags takendown updatedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "appealed" Data.Aeson..?= appealed
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lastAppealedAt" Data.Aeson..?= lastAppealedAt
        , Data.Aeson.Key.fromString "lastReportedAt" Data.Aeson..?= lastReportedAt
        , Data.Aeson.Key.fromString "lastReviewedAt" Data.Aeson..?= lastReviewedAt
        , Data.Aeson.Key.fromString "lastReviewedBy" Data.Aeson..?= lastReviewedBy
        , Data.Aeson.Key.fromString "muteReportingUntil" Data.Aeson..?= muteReportingUntil
        , Data.Aeson.Key.fromString "muteUntil" Data.Aeson..?= muteUntil
        , Data.Aeson.Key.fromString "reviewState" Data.Aeson..= reviewState
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..?= subjectBlobCids
        , Data.Aeson.Key.fromString "subjectRepoHandle" Data.Aeson..?= subjectRepoHandle
        , Data.Aeson.Key.fromString "suspendUntil" Data.Aeson..?= suspendUntil
        , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
        , Data.Aeson.Key.fromString "takendown" Data.Aeson..?= takendown
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]
  toEncoding (SubjectStatusView appealed comment createdAt id' lastAppealedAt lastReportedAt lastReviewedAt lastReviewedBy muteReportingUntil muteUntil reviewState subject subjectBlobCids subjectRepoHandle suspendUntil tags takendown updatedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "appealed" Data.Aeson..?= appealed
        , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "lastAppealedAt" Data.Aeson..?= lastAppealedAt
        , Data.Aeson.Key.fromString "lastReportedAt" Data.Aeson..?= lastReportedAt
        , Data.Aeson.Key.fromString "lastReviewedAt" Data.Aeson..?= lastReviewedAt
        , Data.Aeson.Key.fromString "lastReviewedBy" Data.Aeson..?= lastReviewedBy
        , Data.Aeson.Key.fromString "muteReportingUntil" Data.Aeson..?= muteReportingUntil
        , Data.Aeson.Key.fromString "muteUntil" Data.Aeson..?= muteUntil
        , Data.Aeson.Key.fromString "reviewState" Data.Aeson..= reviewState
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..?= subjectBlobCids
        , Data.Aeson.Key.fromString "subjectRepoHandle" Data.Aeson..?= subjectRepoHandle
        , Data.Aeson.Key.fromString "suspendUntil" Data.Aeson..?= suspendUntil
        , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
        , Data.Aeson.Key.fromString "takendown" Data.Aeson..?= takendown
        , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
        ]

subjectStatusView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SubjectStatusView -> kv
subjectStatusView'AesonFields (SubjectStatusView appealed comment createdAt id' lastAppealedAt lastReportedAt lastReviewedAt lastReviewedBy muteReportingUntil muteUntil reviewState subject subjectBlobCids subjectRepoHandle suspendUntil tags takendown updatedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "appealed" Data.Aeson..?= appealed
    , Data.Aeson.Key.fromString "comment" Data.Aeson..?= comment
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..= createdAt
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "lastAppealedAt" Data.Aeson..?= lastAppealedAt
    , Data.Aeson.Key.fromString "lastReportedAt" Data.Aeson..?= lastReportedAt
    , Data.Aeson.Key.fromString "lastReviewedAt" Data.Aeson..?= lastReviewedAt
    , Data.Aeson.Key.fromString "lastReviewedBy" Data.Aeson..?= lastReviewedBy
    , Data.Aeson.Key.fromString "muteReportingUntil" Data.Aeson..?= muteReportingUntil
    , Data.Aeson.Key.fromString "muteUntil" Data.Aeson..?= muteUntil
    , Data.Aeson.Key.fromString "reviewState" Data.Aeson..= reviewState
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "subjectBlobCids" Data.Aeson..?= subjectBlobCids
    , Data.Aeson.Key.fromString "subjectRepoHandle" Data.Aeson..?= subjectRepoHandle
    , Data.Aeson.Key.fromString "suspendUntil" Data.Aeson..?= suspendUntil
    , Data.Aeson.Key.fromString "tags" Data.Aeson..?= tags
    , Data.Aeson.Key.fromString "takendown" Data.Aeson..?= takendown
    , Data.Aeson.Key.fromString "updatedAt" Data.Aeson..= updatedAt
    ]

data VideoDetails = VideoDetails
  { height :: Integer
  , length :: Integer
  , width :: Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON VideoDetails where
  parseJSON = Data.Aeson.withObject "VideoDetails" $ \v -> do
    height <- v Data.Aeson..: Data.Aeson.Key.fromString "height"
    length <- v Data.Aeson..: Data.Aeson.Key.fromString "length"
    width <- v Data.Aeson..: Data.Aeson.Key.fromString "width"
    pure $ VideoDetails height length width

instance Data.Aeson.ToJSON VideoDetails where
  toJSON (VideoDetails height length width) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "length" Data.Aeson..= length
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]
  toEncoding (VideoDetails height length width) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
        , Data.Aeson.Key.fromString "length" Data.Aeson..= length
        , Data.Aeson.Key.fromString "width" Data.Aeson..= width
        ]

videoDetails'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => VideoDetails -> kv
videoDetails'AesonFields (VideoDetails height length width) =
  mconcat
    [ Data.Aeson.Key.fromString "height" Data.Aeson..= height
    , Data.Aeson.Key.fromString "length" Data.Aeson..= length
    , Data.Aeson.Key.fromString "width" Data.Aeson..= width
    ]

data BlobViewDetailsKind
  = BlobViewDetailsKindImageDetails ImageDetails
  | BlobViewDetailsKindVideoDetails VideoDetails
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BlobViewDetailsKind where
  parseJSON = Data.Aeson.withObject "BlobViewDetailsKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#imageDetails" -> BlobViewDetailsKindImageDetails <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#videoDetails" -> BlobViewDetailsKindVideoDetails <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON BlobViewDetailsKind where
  toJSON = \case
    BlobViewDetailsKindImageDetails v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#imageDetails") <> (Tools.Ozone.Moderation.Defs.imageDetails'AesonFields v))
    BlobViewDetailsKindVideoDetails v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#videoDetails") <> (Tools.Ozone.Moderation.Defs.videoDetails'AesonFields v))
  toEncoding = \case
    BlobViewDetailsKindImageDetails v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#imageDetails") <> (Tools.Ozone.Moderation.Defs.imageDetails'AesonFields v))
    BlobViewDetailsKindVideoDetails v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#videoDetails") <> (Tools.Ozone.Moderation.Defs.videoDetails'AesonFields v))

data ModEventViewDetailEventKind
  = ModEventViewDetailEventKindModEventTakedown ModEventTakedown
  | ModEventViewDetailEventKindModEventReverseTakedown ModEventReverseTakedown
  | ModEventViewDetailEventKindModEventComment ModEventComment
  | ModEventViewDetailEventKindModEventReport ModEventReport
  | ModEventViewDetailEventKindModEventLabel ModEventLabel
  | ModEventViewDetailEventKindModEventAcknowledge ModEventAcknowledge
  | ModEventViewDetailEventKindModEventEscalate ModEventEscalate
  | ModEventViewDetailEventKindModEventMute ModEventMute
  | ModEventViewDetailEventKindModEventUnmute ModEventUnmute
  | ModEventViewDetailEventKindModEventMuteReporter ModEventMuteReporter
  | ModEventViewDetailEventKindModEventUnmuteReporter ModEventUnmuteReporter
  | ModEventViewDetailEventKindModEventEmail ModEventEmail
  | ModEventViewDetailEventKindModEventResolveAppeal ModEventResolveAppeal
  | ModEventViewDetailEventKindModEventDivert ModEventDivert
  | ModEventViewDetailEventKindModEventTag ModEventTag
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventViewDetailEventKind where
  parseJSON = Data.Aeson.withObject "ModEventViewDetailEventKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#modEventTakedown" -> ModEventViewDetailEventKindModEventTakedown <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventReverseTakedown" -> ModEventViewDetailEventKindModEventReverseTakedown <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventComment" -> ModEventViewDetailEventKindModEventComment <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventReport" -> ModEventViewDetailEventKindModEventReport <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventLabel" -> ModEventViewDetailEventKindModEventLabel <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventAcknowledge" -> ModEventViewDetailEventKindModEventAcknowledge <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventEscalate" -> ModEventViewDetailEventKindModEventEscalate <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventMute" -> ModEventViewDetailEventKindModEventMute <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventUnmute" -> ModEventViewDetailEventKindModEventUnmute <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventMuteReporter" -> ModEventViewDetailEventKindModEventMuteReporter <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventUnmuteReporter" -> ModEventViewDetailEventKindModEventUnmuteReporter <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventEmail" -> ModEventViewDetailEventKindModEventEmail <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventResolveAppeal" -> ModEventViewDetailEventKindModEventResolveAppeal <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventDivert" -> ModEventViewDetailEventKindModEventDivert <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventTag" -> ModEventViewDetailEventKindModEventTag <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ModEventViewDetailEventKind where
  toJSON = \case
    ModEventViewDetailEventKindModEventTakedown v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTakedown") <> (Tools.Ozone.Moderation.Defs.modEventTakedown'AesonFields v))
    ModEventViewDetailEventKindModEventReverseTakedown v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReverseTakedown") <> (Tools.Ozone.Moderation.Defs.modEventReverseTakedown'AesonFields v))
    ModEventViewDetailEventKindModEventComment v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventComment") <> (Tools.Ozone.Moderation.Defs.modEventComment'AesonFields v))
    ModEventViewDetailEventKindModEventReport v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReport") <> (Tools.Ozone.Moderation.Defs.modEventReport'AesonFields v))
    ModEventViewDetailEventKindModEventLabel v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventLabel") <> (Tools.Ozone.Moderation.Defs.modEventLabel'AesonFields v))
    ModEventViewDetailEventKindModEventAcknowledge v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventAcknowledge") <> (Tools.Ozone.Moderation.Defs.modEventAcknowledge'AesonFields v))
    ModEventViewDetailEventKindModEventEscalate v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEscalate") <> (Tools.Ozone.Moderation.Defs.modEventEscalate'AesonFields v))
    ModEventViewDetailEventKindModEventMute v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMute") <> (Tools.Ozone.Moderation.Defs.modEventMute'AesonFields v))
    ModEventViewDetailEventKindModEventUnmute v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmute") <> (Tools.Ozone.Moderation.Defs.modEventUnmute'AesonFields v))
    ModEventViewDetailEventKindModEventMuteReporter v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventMuteReporter'AesonFields v))
    ModEventViewDetailEventKindModEventUnmuteReporter v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventUnmuteReporter'AesonFields v))
    ModEventViewDetailEventKindModEventEmail v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEmail") <> (Tools.Ozone.Moderation.Defs.modEventEmail'AesonFields v))
    ModEventViewDetailEventKindModEventResolveAppeal v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventResolveAppeal") <> (Tools.Ozone.Moderation.Defs.modEventResolveAppeal'AesonFields v))
    ModEventViewDetailEventKindModEventDivert v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventDivert") <> (Tools.Ozone.Moderation.Defs.modEventDivert'AesonFields v))
    ModEventViewDetailEventKindModEventTag v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTag") <> (Tools.Ozone.Moderation.Defs.modEventTag'AesonFields v))
  toEncoding = \case
    ModEventViewDetailEventKindModEventTakedown v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTakedown") <> (Tools.Ozone.Moderation.Defs.modEventTakedown'AesonFields v))
    ModEventViewDetailEventKindModEventReverseTakedown v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReverseTakedown") <> (Tools.Ozone.Moderation.Defs.modEventReverseTakedown'AesonFields v))
    ModEventViewDetailEventKindModEventComment v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventComment") <> (Tools.Ozone.Moderation.Defs.modEventComment'AesonFields v))
    ModEventViewDetailEventKindModEventReport v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReport") <> (Tools.Ozone.Moderation.Defs.modEventReport'AesonFields v))
    ModEventViewDetailEventKindModEventLabel v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventLabel") <> (Tools.Ozone.Moderation.Defs.modEventLabel'AesonFields v))
    ModEventViewDetailEventKindModEventAcknowledge v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventAcknowledge") <> (Tools.Ozone.Moderation.Defs.modEventAcknowledge'AesonFields v))
    ModEventViewDetailEventKindModEventEscalate v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEscalate") <> (Tools.Ozone.Moderation.Defs.modEventEscalate'AesonFields v))
    ModEventViewDetailEventKindModEventMute v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMute") <> (Tools.Ozone.Moderation.Defs.modEventMute'AesonFields v))
    ModEventViewDetailEventKindModEventUnmute v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmute") <> (Tools.Ozone.Moderation.Defs.modEventUnmute'AesonFields v))
    ModEventViewDetailEventKindModEventMuteReporter v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventMuteReporter'AesonFields v))
    ModEventViewDetailEventKindModEventUnmuteReporter v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventUnmuteReporter'AesonFields v))
    ModEventViewDetailEventKindModEventEmail v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEmail") <> (Tools.Ozone.Moderation.Defs.modEventEmail'AesonFields v))
    ModEventViewDetailEventKindModEventResolveAppeal v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventResolveAppeal") <> (Tools.Ozone.Moderation.Defs.modEventResolveAppeal'AesonFields v))
    ModEventViewDetailEventKindModEventDivert v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventDivert") <> (Tools.Ozone.Moderation.Defs.modEventDivert'AesonFields v))
    ModEventViewDetailEventKindModEventTag v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTag") <> (Tools.Ozone.Moderation.Defs.modEventTag'AesonFields v))

data ModEventViewDetailSubjectKind
  = ModEventViewDetailSubjectKindRepoView RepoView
  | ModEventViewDetailSubjectKindRepoViewNotFound RepoViewNotFound
  | ModEventViewDetailSubjectKindRecordView RecordView
  | ModEventViewDetailSubjectKindRecordViewNotFound RecordViewNotFound
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventViewDetailSubjectKind where
  parseJSON = Data.Aeson.withObject "ModEventViewDetailSubjectKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#repoView" -> ModEventViewDetailSubjectKindRepoView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#repoViewNotFound" -> ModEventViewDetailSubjectKindRepoViewNotFound <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#recordView" -> ModEventViewDetailSubjectKindRecordView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#recordViewNotFound" -> ModEventViewDetailSubjectKindRecordViewNotFound <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ModEventViewDetailSubjectKind where
  toJSON = \case
    ModEventViewDetailSubjectKindRepoView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoView") <> (Tools.Ozone.Moderation.Defs.repoView'AesonFields v))
    ModEventViewDetailSubjectKindRepoViewNotFound v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewNotFound") <> (Tools.Ozone.Moderation.Defs.repoViewNotFound'AesonFields v))
    ModEventViewDetailSubjectKindRecordView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordView") <> (Tools.Ozone.Moderation.Defs.recordView'AesonFields v))
    ModEventViewDetailSubjectKindRecordViewNotFound v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewNotFound") <> (Tools.Ozone.Moderation.Defs.recordViewNotFound'AesonFields v))
  toEncoding = \case
    ModEventViewDetailSubjectKindRepoView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoView") <> (Tools.Ozone.Moderation.Defs.repoView'AesonFields v))
    ModEventViewDetailSubjectKindRepoViewNotFound v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewNotFound") <> (Tools.Ozone.Moderation.Defs.repoViewNotFound'AesonFields v))
    ModEventViewDetailSubjectKindRecordView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordView") <> (Tools.Ozone.Moderation.Defs.recordView'AesonFields v))
    ModEventViewDetailSubjectKindRecordViewNotFound v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#recordViewNotFound") <> (Tools.Ozone.Moderation.Defs.recordViewNotFound'AesonFields v))

data ModEventViewEventKind
  = ModEventViewEventKindModEventTakedown ModEventTakedown
  | ModEventViewEventKindModEventReverseTakedown ModEventReverseTakedown
  | ModEventViewEventKindModEventComment ModEventComment
  | ModEventViewEventKindModEventReport ModEventReport
  | ModEventViewEventKindModEventLabel ModEventLabel
  | ModEventViewEventKindModEventAcknowledge ModEventAcknowledge
  | ModEventViewEventKindModEventEscalate ModEventEscalate
  | ModEventViewEventKindModEventMute ModEventMute
  | ModEventViewEventKindModEventUnmute ModEventUnmute
  | ModEventViewEventKindModEventMuteReporter ModEventMuteReporter
  | ModEventViewEventKindModEventUnmuteReporter ModEventUnmuteReporter
  | ModEventViewEventKindModEventEmail ModEventEmail
  | ModEventViewEventKindModEventResolveAppeal ModEventResolveAppeal
  | ModEventViewEventKindModEventDivert ModEventDivert
  | ModEventViewEventKindModEventTag ModEventTag
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventViewEventKind where
  parseJSON = Data.Aeson.withObject "ModEventViewEventKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#modEventTakedown" -> ModEventViewEventKindModEventTakedown <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventReverseTakedown" -> ModEventViewEventKindModEventReverseTakedown <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventComment" -> ModEventViewEventKindModEventComment <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventReport" -> ModEventViewEventKindModEventReport <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventLabel" -> ModEventViewEventKindModEventLabel <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventAcknowledge" -> ModEventViewEventKindModEventAcknowledge <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventEscalate" -> ModEventViewEventKindModEventEscalate <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventMute" -> ModEventViewEventKindModEventMute <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventUnmute" -> ModEventViewEventKindModEventUnmute <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventMuteReporter" -> ModEventViewEventKindModEventMuteReporter <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventUnmuteReporter" -> ModEventViewEventKindModEventUnmuteReporter <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventEmail" -> ModEventViewEventKindModEventEmail <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventResolveAppeal" -> ModEventViewEventKindModEventResolveAppeal <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventDivert" -> ModEventViewEventKindModEventDivert <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#modEventTag" -> ModEventViewEventKindModEventTag <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ModEventViewEventKind where
  toJSON = \case
    ModEventViewEventKindModEventTakedown v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTakedown") <> (Tools.Ozone.Moderation.Defs.modEventTakedown'AesonFields v))
    ModEventViewEventKindModEventReverseTakedown v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReverseTakedown") <> (Tools.Ozone.Moderation.Defs.modEventReverseTakedown'AesonFields v))
    ModEventViewEventKindModEventComment v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventComment") <> (Tools.Ozone.Moderation.Defs.modEventComment'AesonFields v))
    ModEventViewEventKindModEventReport v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReport") <> (Tools.Ozone.Moderation.Defs.modEventReport'AesonFields v))
    ModEventViewEventKindModEventLabel v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventLabel") <> (Tools.Ozone.Moderation.Defs.modEventLabel'AesonFields v))
    ModEventViewEventKindModEventAcknowledge v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventAcknowledge") <> (Tools.Ozone.Moderation.Defs.modEventAcknowledge'AesonFields v))
    ModEventViewEventKindModEventEscalate v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEscalate") <> (Tools.Ozone.Moderation.Defs.modEventEscalate'AesonFields v))
    ModEventViewEventKindModEventMute v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMute") <> (Tools.Ozone.Moderation.Defs.modEventMute'AesonFields v))
    ModEventViewEventKindModEventUnmute v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmute") <> (Tools.Ozone.Moderation.Defs.modEventUnmute'AesonFields v))
    ModEventViewEventKindModEventMuteReporter v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventMuteReporter'AesonFields v))
    ModEventViewEventKindModEventUnmuteReporter v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventUnmuteReporter'AesonFields v))
    ModEventViewEventKindModEventEmail v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEmail") <> (Tools.Ozone.Moderation.Defs.modEventEmail'AesonFields v))
    ModEventViewEventKindModEventResolveAppeal v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventResolveAppeal") <> (Tools.Ozone.Moderation.Defs.modEventResolveAppeal'AesonFields v))
    ModEventViewEventKindModEventDivert v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventDivert") <> (Tools.Ozone.Moderation.Defs.modEventDivert'AesonFields v))
    ModEventViewEventKindModEventTag v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTag") <> (Tools.Ozone.Moderation.Defs.modEventTag'AesonFields v))
  toEncoding = \case
    ModEventViewEventKindModEventTakedown v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTakedown") <> (Tools.Ozone.Moderation.Defs.modEventTakedown'AesonFields v))
    ModEventViewEventKindModEventReverseTakedown v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReverseTakedown") <> (Tools.Ozone.Moderation.Defs.modEventReverseTakedown'AesonFields v))
    ModEventViewEventKindModEventComment v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventComment") <> (Tools.Ozone.Moderation.Defs.modEventComment'AesonFields v))
    ModEventViewEventKindModEventReport v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventReport") <> (Tools.Ozone.Moderation.Defs.modEventReport'AesonFields v))
    ModEventViewEventKindModEventLabel v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventLabel") <> (Tools.Ozone.Moderation.Defs.modEventLabel'AesonFields v))
    ModEventViewEventKindModEventAcknowledge v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventAcknowledge") <> (Tools.Ozone.Moderation.Defs.modEventAcknowledge'AesonFields v))
    ModEventViewEventKindModEventEscalate v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEscalate") <> (Tools.Ozone.Moderation.Defs.modEventEscalate'AesonFields v))
    ModEventViewEventKindModEventMute v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMute") <> (Tools.Ozone.Moderation.Defs.modEventMute'AesonFields v))
    ModEventViewEventKindModEventUnmute v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmute") <> (Tools.Ozone.Moderation.Defs.modEventUnmute'AesonFields v))
    ModEventViewEventKindModEventMuteReporter v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventMuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventMuteReporter'AesonFields v))
    ModEventViewEventKindModEventUnmuteReporter v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventUnmuteReporter") <> (Tools.Ozone.Moderation.Defs.modEventUnmuteReporter'AesonFields v))
    ModEventViewEventKindModEventEmail v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventEmail") <> (Tools.Ozone.Moderation.Defs.modEventEmail'AesonFields v))
    ModEventViewEventKindModEventResolveAppeal v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventResolveAppeal") <> (Tools.Ozone.Moderation.Defs.modEventResolveAppeal'AesonFields v))
    ModEventViewEventKindModEventDivert v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventDivert") <> (Tools.Ozone.Moderation.Defs.modEventDivert'AesonFields v))
    ModEventViewEventKindModEventTag v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#modEventTag") <> (Tools.Ozone.Moderation.Defs.modEventTag'AesonFields v))

data ModEventViewSubjectKind
  = ModEventViewSubjectKindRepoRef Com.Atproto.Admin.Defs.RepoRef
  | ModEventViewSubjectKindStrongRef Com.Atproto.Repo.StrongRef.StrongRef
  | ModEventViewSubjectKindMessageRef Chat.Bsky.Convo.Defs.MessageRef
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ModEventViewSubjectKind where
  parseJSON = Data.Aeson.withObject "ModEventViewSubjectKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.admin.defs#repoRef" -> ModEventViewSubjectKindRepoRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "com.atproto.repo.strongRef#strongRef" -> ModEventViewSubjectKindStrongRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "chat.bsky.convo.defs#messageRef" -> ModEventViewSubjectKindMessageRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ModEventViewSubjectKind where
  toJSON = \case
    ModEventViewSubjectKindRepoRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    ModEventViewSubjectKindStrongRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
    ModEventViewSubjectKindMessageRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageRef") <> (Chat.Bsky.Convo.Defs.messageRef'AesonFields v))
  toEncoding = \case
    ModEventViewSubjectKindRepoRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    ModEventViewSubjectKindStrongRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
    ModEventViewSubjectKindMessageRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "chat.bsky.convo.defs#messageRef") <> (Chat.Bsky.Convo.Defs.messageRef'AesonFields v))

data SubjectStatusViewSubjectKind
  = SubjectStatusViewSubjectKindRepoRef Com.Atproto.Admin.Defs.RepoRef
  | SubjectStatusViewSubjectKindStrongRef Com.Atproto.Repo.StrongRef.StrongRef
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SubjectStatusViewSubjectKind where
  parseJSON = Data.Aeson.withObject "SubjectStatusViewSubjectKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.admin.defs#repoRef" -> SubjectStatusViewSubjectKindRepoRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "com.atproto.repo.strongRef#strongRef" -> SubjectStatusViewSubjectKindStrongRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON SubjectStatusViewSubjectKind where
  toJSON = \case
    SubjectStatusViewSubjectKindRepoRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    SubjectStatusViewSubjectKindStrongRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
  toEncoding = \case
    SubjectStatusViewSubjectKindRepoRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    SubjectStatusViewSubjectKindStrongRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
