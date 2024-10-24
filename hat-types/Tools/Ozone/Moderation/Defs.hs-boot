module Tools.Ozone.Moderation.Defs where

import qualified Data.Aeson

data BlobView

instance Show BlobView
instance Read BlobView
instance Eq BlobView
instance Ord BlobView
instance Data.Aeson.FromJSON BlobView

instance Data.Aeson.ToJSON BlobView

blobView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlobView -> kv
data ImageDetails

instance Show ImageDetails
instance Read ImageDetails
instance Eq ImageDetails
instance Ord ImageDetails
instance Data.Aeson.FromJSON ImageDetails

instance Data.Aeson.ToJSON ImageDetails

imageDetails'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ImageDetails -> kv
data ModEventAcknowledge

instance Show ModEventAcknowledge
instance Read ModEventAcknowledge
instance Eq ModEventAcknowledge
instance Ord ModEventAcknowledge
instance Data.Aeson.FromJSON ModEventAcknowledge

instance Data.Aeson.ToJSON ModEventAcknowledge

modEventAcknowledge'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventAcknowledge -> kv
data ModEventComment

instance Show ModEventComment
instance Read ModEventComment
instance Eq ModEventComment
instance Ord ModEventComment
instance Data.Aeson.FromJSON ModEventComment

instance Data.Aeson.ToJSON ModEventComment

modEventComment'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventComment -> kv
data ModEventDivert

instance Show ModEventDivert
instance Read ModEventDivert
instance Eq ModEventDivert
instance Ord ModEventDivert
instance Data.Aeson.FromJSON ModEventDivert

instance Data.Aeson.ToJSON ModEventDivert

modEventDivert'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventDivert -> kv
data ModEventEmail

instance Show ModEventEmail
instance Read ModEventEmail
instance Eq ModEventEmail
instance Ord ModEventEmail
instance Data.Aeson.FromJSON ModEventEmail

instance Data.Aeson.ToJSON ModEventEmail

modEventEmail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventEmail -> kv
data ModEventEscalate

instance Show ModEventEscalate
instance Read ModEventEscalate
instance Eq ModEventEscalate
instance Ord ModEventEscalate
instance Data.Aeson.FromJSON ModEventEscalate

instance Data.Aeson.ToJSON ModEventEscalate

modEventEscalate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventEscalate -> kv
data ModEventLabel

instance Show ModEventLabel
instance Read ModEventLabel
instance Eq ModEventLabel
instance Ord ModEventLabel
instance Data.Aeson.FromJSON ModEventLabel

instance Data.Aeson.ToJSON ModEventLabel

modEventLabel'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventLabel -> kv
data ModEventMute

instance Show ModEventMute
instance Read ModEventMute
instance Eq ModEventMute
instance Ord ModEventMute
instance Data.Aeson.FromJSON ModEventMute

instance Data.Aeson.ToJSON ModEventMute

modEventMute'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventMute -> kv
data ModEventMuteReporter

instance Show ModEventMuteReporter
instance Read ModEventMuteReporter
instance Eq ModEventMuteReporter
instance Ord ModEventMuteReporter
instance Data.Aeson.FromJSON ModEventMuteReporter

instance Data.Aeson.ToJSON ModEventMuteReporter

modEventMuteReporter'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventMuteReporter -> kv
data ModEventReport

instance Show ModEventReport
instance Read ModEventReport
instance Eq ModEventReport
instance Ord ModEventReport
instance Data.Aeson.FromJSON ModEventReport

instance Data.Aeson.ToJSON ModEventReport

modEventReport'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventReport -> kv
data ModEventResolveAppeal

instance Show ModEventResolveAppeal
instance Read ModEventResolveAppeal
instance Eq ModEventResolveAppeal
instance Ord ModEventResolveAppeal
instance Data.Aeson.FromJSON ModEventResolveAppeal

instance Data.Aeson.ToJSON ModEventResolveAppeal

modEventResolveAppeal'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventResolveAppeal -> kv
data ModEventReverseTakedown

instance Show ModEventReverseTakedown
instance Read ModEventReverseTakedown
instance Eq ModEventReverseTakedown
instance Ord ModEventReverseTakedown
instance Data.Aeson.FromJSON ModEventReverseTakedown

instance Data.Aeson.ToJSON ModEventReverseTakedown

modEventReverseTakedown'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventReverseTakedown -> kv
data ModEventTag

instance Show ModEventTag
instance Read ModEventTag
instance Eq ModEventTag
instance Ord ModEventTag
instance Data.Aeson.FromJSON ModEventTag

instance Data.Aeson.ToJSON ModEventTag

modEventTag'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventTag -> kv
data ModEventTakedown

instance Show ModEventTakedown
instance Read ModEventTakedown
instance Eq ModEventTakedown
instance Ord ModEventTakedown
instance Data.Aeson.FromJSON ModEventTakedown

instance Data.Aeson.ToJSON ModEventTakedown

modEventTakedown'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventTakedown -> kv
data ModEventUnmute

instance Show ModEventUnmute
instance Read ModEventUnmute
instance Eq ModEventUnmute
instance Ord ModEventUnmute
instance Data.Aeson.FromJSON ModEventUnmute

instance Data.Aeson.ToJSON ModEventUnmute

modEventUnmute'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventUnmute -> kv
data ModEventUnmuteReporter

instance Show ModEventUnmuteReporter
instance Read ModEventUnmuteReporter
instance Eq ModEventUnmuteReporter
instance Ord ModEventUnmuteReporter
instance Data.Aeson.FromJSON ModEventUnmuteReporter

instance Data.Aeson.ToJSON ModEventUnmuteReporter

modEventUnmuteReporter'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventUnmuteReporter -> kv
data ModEventView

instance Show ModEventView
instance Read ModEventView
instance Eq ModEventView
instance Ord ModEventView
instance Data.Aeson.FromJSON ModEventView

instance Data.Aeson.ToJSON ModEventView

modEventView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventView -> kv
data ModEventViewDetail

instance Show ModEventViewDetail
instance Read ModEventViewDetail
instance Eq ModEventViewDetail
instance Ord ModEventViewDetail
instance Data.Aeson.FromJSON ModEventViewDetail

instance Data.Aeson.ToJSON ModEventViewDetail

modEventViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModEventViewDetail -> kv
data Moderation

instance Show Moderation
instance Read Moderation
instance Eq Moderation
instance Ord Moderation
instance Data.Aeson.FromJSON Moderation

instance Data.Aeson.ToJSON Moderation

moderation'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Moderation -> kv
data ModerationDetail

instance Show ModerationDetail
instance Read ModerationDetail
instance Eq ModerationDetail
instance Ord ModerationDetail
instance Data.Aeson.FromJSON ModerationDetail

instance Data.Aeson.ToJSON ModerationDetail

moderationDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ModerationDetail -> kv
data RecordView

instance Show RecordView
instance Read RecordView
instance Eq RecordView
instance Ord RecordView
instance Data.Aeson.FromJSON RecordView

instance Data.Aeson.ToJSON RecordView

recordView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordView -> kv
data RecordViewDetail

instance Show RecordViewDetail
instance Read RecordViewDetail
instance Eq RecordViewDetail
instance Ord RecordViewDetail
instance Data.Aeson.FromJSON RecordViewDetail

instance Data.Aeson.ToJSON RecordViewDetail

recordViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordViewDetail -> kv
data RecordViewNotFound

instance Show RecordViewNotFound
instance Read RecordViewNotFound
instance Eq RecordViewNotFound
instance Ord RecordViewNotFound
instance Data.Aeson.FromJSON RecordViewNotFound

instance Data.Aeson.ToJSON RecordViewNotFound

recordViewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RecordViewNotFound -> kv
data RepoView

instance Show RepoView
instance Read RepoView
instance Eq RepoView
instance Ord RepoView
instance Data.Aeson.FromJSON RepoView

instance Data.Aeson.ToJSON RepoView

repoView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoView -> kv
data RepoViewDetail

instance Show RepoViewDetail
instance Read RepoViewDetail
instance Eq RepoViewDetail
instance Ord RepoViewDetail
instance Data.Aeson.FromJSON RepoViewDetail

instance Data.Aeson.ToJSON RepoViewDetail

repoViewDetail'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoViewDetail -> kv
data RepoViewNotFound

instance Show RepoViewNotFound
instance Read RepoViewNotFound
instance Eq RepoViewNotFound
instance Ord RepoViewNotFound
instance Data.Aeson.FromJSON RepoViewNotFound

instance Data.Aeson.ToJSON RepoViewNotFound

repoViewNotFound'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoViewNotFound -> kv
data SubjectReviewState

instance Show SubjectReviewState
instance Read SubjectReviewState
instance Eq SubjectReviewState
instance Ord SubjectReviewState
instance Data.Aeson.FromJSON SubjectReviewState

instance Data.Aeson.ToJSON SubjectReviewState

data SubjectStatusView

instance Show SubjectStatusView
instance Read SubjectStatusView
instance Eq SubjectStatusView
instance Ord SubjectStatusView
instance Data.Aeson.FromJSON SubjectStatusView

instance Data.Aeson.ToJSON SubjectStatusView

subjectStatusView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SubjectStatusView -> kv
data VideoDetails

instance Show VideoDetails
instance Read VideoDetails
instance Eq VideoDetails
instance Ord VideoDetails
instance Data.Aeson.FromJSON VideoDetails

instance Data.Aeson.ToJSON VideoDetails

videoDetails'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => VideoDetails -> kv
data BlobViewDetailsKind

instance Show BlobViewDetailsKind
instance Read BlobViewDetailsKind
instance Eq BlobViewDetailsKind
instance Ord BlobViewDetailsKind
instance Data.Aeson.FromJSON BlobViewDetailsKind

instance Data.Aeson.ToJSON BlobViewDetailsKind

data ModEventViewDetailEventKind

instance Show ModEventViewDetailEventKind
instance Read ModEventViewDetailEventKind
instance Eq ModEventViewDetailEventKind
instance Ord ModEventViewDetailEventKind
instance Data.Aeson.FromJSON ModEventViewDetailEventKind

instance Data.Aeson.ToJSON ModEventViewDetailEventKind

data ModEventViewDetailSubjectKind

instance Show ModEventViewDetailSubjectKind
instance Read ModEventViewDetailSubjectKind
instance Eq ModEventViewDetailSubjectKind
instance Ord ModEventViewDetailSubjectKind
instance Data.Aeson.FromJSON ModEventViewDetailSubjectKind

instance Data.Aeson.ToJSON ModEventViewDetailSubjectKind

data ModEventViewEventKind

instance Show ModEventViewEventKind
instance Read ModEventViewEventKind
instance Eq ModEventViewEventKind
instance Ord ModEventViewEventKind
instance Data.Aeson.FromJSON ModEventViewEventKind

instance Data.Aeson.ToJSON ModEventViewEventKind

data ModEventViewSubjectKind

instance Show ModEventViewSubjectKind
instance Read ModEventViewSubjectKind
instance Eq ModEventViewSubjectKind
instance Ord ModEventViewSubjectKind
instance Data.Aeson.FromJSON ModEventViewSubjectKind

instance Data.Aeson.ToJSON ModEventViewSubjectKind

data SubjectStatusViewSubjectKind

instance Show SubjectStatusViewSubjectKind
instance Read SubjectStatusViewSubjectKind
instance Eq SubjectStatusViewSubjectKind
instance Ord SubjectStatusViewSubjectKind
instance Data.Aeson.FromJSON SubjectStatusViewSubjectKind

instance Data.Aeson.ToJSON SubjectStatusViewSubjectKind
