{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Actor.Defs where

import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data AdultContentPref = AdultContentPref
  { enabled :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON AdultContentPref where
  parseJSON = Data.Aeson.withObject "AdultContentPref" $ \v -> do
    enabled <- v Data.Aeson..: Data.Aeson.Key.fromString "enabled"
    pure $ AdultContentPref enabled

instance Data.Aeson.ToJSON AdultContentPref where
  toJSON (AdultContentPref enabled) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "enabled" Data.Aeson..= enabled
        ]
  toEncoding (AdultContentPref enabled) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "enabled" Data.Aeson..= enabled
        ]

adultContentPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AdultContentPref -> kv
adultContentPref'AesonFields (AdultContentPref enabled) =
  mconcat
    [ Data.Aeson.Key.fromString "enabled" Data.Aeson..= enabled
    ]

data BskyAppProgressGuide = BskyAppProgressGuide
  { guide :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BskyAppProgressGuide where
  parseJSON = Data.Aeson.withObject "BskyAppProgressGuide" $ \v -> do
    guide <- v Data.Aeson..: Data.Aeson.Key.fromString "guide"
    pure $ BskyAppProgressGuide guide

instance Data.Aeson.ToJSON BskyAppProgressGuide where
  toJSON (BskyAppProgressGuide guide) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "guide" Data.Aeson..= guide
        ]
  toEncoding (BskyAppProgressGuide guide) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "guide" Data.Aeson..= guide
        ]

bskyAppProgressGuide'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BskyAppProgressGuide -> kv
bskyAppProgressGuide'AesonFields (BskyAppProgressGuide guide) =
  mconcat
    [ Data.Aeson.Key.fromString "guide" Data.Aeson..= guide
    ]

data BskyAppStatePref = BskyAppStatePref
  { activeProgressGuide :: Maybe BskyAppProgressGuide
  , nuxs :: Maybe [Nux]
  , queuedNudges :: Maybe [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BskyAppStatePref where
  parseJSON = Data.Aeson.withObject "BskyAppStatePref" $ \v -> do
    activeProgressGuide <- v Data.Aeson..:? Data.Aeson.Key.fromString "activeProgressGuide"
    nuxs <- v Data.Aeson..:? Data.Aeson.Key.fromString "nuxs"
    queuedNudges <- v Data.Aeson..:? Data.Aeson.Key.fromString "queuedNudges"
    pure $ BskyAppStatePref activeProgressGuide nuxs queuedNudges

instance Data.Aeson.ToJSON BskyAppStatePref where
  toJSON (BskyAppStatePref activeProgressGuide nuxs queuedNudges) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "activeProgressGuide" Data.Aeson..?= activeProgressGuide
        , Data.Aeson.Key.fromString "nuxs" Data.Aeson..?= nuxs
        , Data.Aeson.Key.fromString "queuedNudges" Data.Aeson..?= queuedNudges
        ]
  toEncoding (BskyAppStatePref activeProgressGuide nuxs queuedNudges) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "activeProgressGuide" Data.Aeson..?= activeProgressGuide
        , Data.Aeson.Key.fromString "nuxs" Data.Aeson..?= nuxs
        , Data.Aeson.Key.fromString "queuedNudges" Data.Aeson..?= queuedNudges
        ]

bskyAppStatePref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BskyAppStatePref -> kv
bskyAppStatePref'AesonFields (BskyAppStatePref activeProgressGuide nuxs queuedNudges) =
  mconcat
    [ Data.Aeson.Key.fromString "activeProgressGuide" Data.Aeson..?= activeProgressGuide
    , Data.Aeson.Key.fromString "nuxs" Data.Aeson..?= nuxs
    , Data.Aeson.Key.fromString "queuedNudges" Data.Aeson..?= queuedNudges
    ]

data ContentLabelPref = ContentLabelPref
  { label :: Data.Text.Text
  , labelerDid :: Maybe Data.Text.Text
  , visibility :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ContentLabelPref where
  parseJSON = Data.Aeson.withObject "ContentLabelPref" $ \v -> do
    label <- v Data.Aeson..: Data.Aeson.Key.fromString "label"
    labelerDid <- v Data.Aeson..:? Data.Aeson.Key.fromString "labelerDid"
    visibility <- v Data.Aeson..: Data.Aeson.Key.fromString "visibility"
    pure $ ContentLabelPref label labelerDid visibility

instance Data.Aeson.ToJSON ContentLabelPref where
  toJSON (ContentLabelPref label labelerDid visibility) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "label" Data.Aeson..= label
        , Data.Aeson.Key.fromString "labelerDid" Data.Aeson..?= labelerDid
        , Data.Aeson.Key.fromString "visibility" Data.Aeson..= visibility
        ]
  toEncoding (ContentLabelPref label labelerDid visibility) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "label" Data.Aeson..= label
        , Data.Aeson.Key.fromString "labelerDid" Data.Aeson..?= labelerDid
        , Data.Aeson.Key.fromString "visibility" Data.Aeson..= visibility
        ]

contentLabelPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ContentLabelPref -> kv
contentLabelPref'AesonFields (ContentLabelPref label labelerDid visibility) =
  mconcat
    [ Data.Aeson.Key.fromString "label" Data.Aeson..= label
    , Data.Aeson.Key.fromString "labelerDid" Data.Aeson..?= labelerDid
    , Data.Aeson.Key.fromString "visibility" Data.Aeson..= visibility
    ]

data FeedViewPref = FeedViewPref
  { feed :: Data.Text.Text
  , hideQuotePosts :: Maybe Bool
  , hideReplies :: Maybe Bool
  , hideRepliesByLikeCount :: Maybe Integer
  , hideRepliesByUnfollowed :: Maybe Bool
  , hideReposts :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FeedViewPref where
  parseJSON = Data.Aeson.withObject "FeedViewPref" $ \v -> do
    feed <- v Data.Aeson..: Data.Aeson.Key.fromString "feed"
    hideQuotePosts <- v Data.Aeson..:? Data.Aeson.Key.fromString "hideQuotePosts"
    hideReplies <- v Data.Aeson..:? Data.Aeson.Key.fromString "hideReplies"
    hideRepliesByLikeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "hideRepliesByLikeCount"
    hideRepliesByUnfollowed <- v Data.Aeson..:? Data.Aeson.Key.fromString "hideRepliesByUnfollowed"
    hideReposts <- v Data.Aeson..:? Data.Aeson.Key.fromString "hideReposts"
    pure $ FeedViewPref feed hideQuotePosts hideReplies hideRepliesByLikeCount hideRepliesByUnfollowed hideReposts

instance Data.Aeson.ToJSON FeedViewPref where
  toJSON (FeedViewPref feed hideQuotePosts hideReplies hideRepliesByLikeCount hideRepliesByUnfollowed hideReposts) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        , Data.Aeson.Key.fromString "hideQuotePosts" Data.Aeson..?= hideQuotePosts
        , Data.Aeson.Key.fromString "hideReplies" Data.Aeson..?= hideReplies
        , Data.Aeson.Key.fromString "hideRepliesByLikeCount" Data.Aeson..?= hideRepliesByLikeCount
        , Data.Aeson.Key.fromString "hideRepliesByUnfollowed" Data.Aeson..?= hideRepliesByUnfollowed
        , Data.Aeson.Key.fromString "hideReposts" Data.Aeson..?= hideReposts
        ]
  toEncoding (FeedViewPref feed hideQuotePosts hideReplies hideRepliesByLikeCount hideRepliesByUnfollowed hideReposts) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
        , Data.Aeson.Key.fromString "hideQuotePosts" Data.Aeson..?= hideQuotePosts
        , Data.Aeson.Key.fromString "hideReplies" Data.Aeson..?= hideReplies
        , Data.Aeson.Key.fromString "hideRepliesByLikeCount" Data.Aeson..?= hideRepliesByLikeCount
        , Data.Aeson.Key.fromString "hideRepliesByUnfollowed" Data.Aeson..?= hideRepliesByUnfollowed
        , Data.Aeson.Key.fromString "hideReposts" Data.Aeson..?= hideReposts
        ]

feedViewPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedViewPref -> kv
feedViewPref'AesonFields (FeedViewPref feed hideQuotePosts hideReplies hideRepliesByLikeCount hideRepliesByUnfollowed hideReposts) =
  mconcat
    [ Data.Aeson.Key.fromString "feed" Data.Aeson..= feed
    , Data.Aeson.Key.fromString "hideQuotePosts" Data.Aeson..?= hideQuotePosts
    , Data.Aeson.Key.fromString "hideReplies" Data.Aeson..?= hideReplies
    , Data.Aeson.Key.fromString "hideRepliesByLikeCount" Data.Aeson..?= hideRepliesByLikeCount
    , Data.Aeson.Key.fromString "hideRepliesByUnfollowed" Data.Aeson..?= hideRepliesByUnfollowed
    , Data.Aeson.Key.fromString "hideReposts" Data.Aeson..?= hideReposts
    ]

data HiddenPostsPref = HiddenPostsPref
  { items :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON HiddenPostsPref where
  parseJSON = Data.Aeson.withObject "HiddenPostsPref" $ \v -> do
    items <- v Data.Aeson..: Data.Aeson.Key.fromString "items"
    pure $ HiddenPostsPref items

instance Data.Aeson.ToJSON HiddenPostsPref where
  toJSON (HiddenPostsPref items) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]
  toEncoding (HiddenPostsPref items) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]

hiddenPostsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => HiddenPostsPref -> kv
hiddenPostsPref'AesonFields (HiddenPostsPref items) =
  mconcat
    [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
    ]

data InterestsPref = InterestsPref
  { tags :: [Data.Text.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON InterestsPref where
  parseJSON = Data.Aeson.withObject "InterestsPref" $ \v -> do
    tags <- v Data.Aeson..: Data.Aeson.Key.fromString "tags"
    pure $ InterestsPref tags

instance Data.Aeson.ToJSON InterestsPref where
  toJSON (InterestsPref tags) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "tags" Data.Aeson..= tags
        ]
  toEncoding (InterestsPref tags) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "tags" Data.Aeson..= tags
        ]

interestsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InterestsPref -> kv
interestsPref'AesonFields (InterestsPref tags) =
  mconcat
    [ Data.Aeson.Key.fromString "tags" Data.Aeson..= tags
    ]

data KnownFollowers = KnownFollowers
  { count :: Integer
  , followers :: [ProfileViewBasic]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON KnownFollowers where
  parseJSON = Data.Aeson.withObject "KnownFollowers" $ \v -> do
    count <- v Data.Aeson..: Data.Aeson.Key.fromString "count"
    followers <- v Data.Aeson..: Data.Aeson.Key.fromString "followers"
    pure $ KnownFollowers count followers

instance Data.Aeson.ToJSON KnownFollowers where
  toJSON (KnownFollowers count followers) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
        , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
        ]
  toEncoding (KnownFollowers count followers) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
        , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
        ]

knownFollowers'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => KnownFollowers -> kv
knownFollowers'AesonFields (KnownFollowers count followers) =
  mconcat
    [ Data.Aeson.Key.fromString "count" Data.Aeson..= count
    , Data.Aeson.Key.fromString "followers" Data.Aeson..= followers
    ]

data LabelerPrefItem = LabelerPrefItem
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelerPrefItem where
  parseJSON = Data.Aeson.withObject "LabelerPrefItem" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ LabelerPrefItem did

instance Data.Aeson.ToJSON LabelerPrefItem where
  toJSON (LabelerPrefItem did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (LabelerPrefItem did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

labelerPrefItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerPrefItem -> kv
labelerPrefItem'AesonFields (LabelerPrefItem did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

data LabelersPref = LabelersPref
  { labelers :: [LabelerPrefItem]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON LabelersPref where
  parseJSON = Data.Aeson.withObject "LabelersPref" $ \v -> do
    labelers <- v Data.Aeson..: Data.Aeson.Key.fromString "labelers"
    pure $ LabelersPref labelers

instance Data.Aeson.ToJSON LabelersPref where
  toJSON (LabelersPref labelers) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "labelers" Data.Aeson..= labelers
        ]
  toEncoding (LabelersPref labelers) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "labelers" Data.Aeson..= labelers
        ]

labelersPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelersPref -> kv
labelersPref'AesonFields (LabelersPref labelers) =
  mconcat
    [ Data.Aeson.Key.fromString "labelers" Data.Aeson..= labelers
    ]

data MutedWord = MutedWord
  { actorTarget :: Maybe Data.Text.Text
  , expiresAt :: Maybe Data.Text.Text
  , id' :: Maybe Data.Text.Text
  , targets :: [MutedWordTarget]
  , value :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MutedWord where
  parseJSON = Data.Aeson.withObject "MutedWord" $ \v -> do
    actorTarget <- v Data.Aeson..:? Data.Aeson.Key.fromString "actorTarget"
    expiresAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "expiresAt"
    id' <- v Data.Aeson..:? Data.Aeson.Key.fromString "id"
    targets <- v Data.Aeson..: Data.Aeson.Key.fromString "targets"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ MutedWord actorTarget expiresAt id' targets value

instance Data.Aeson.ToJSON MutedWord where
  toJSON (MutedWord actorTarget expiresAt id' targets value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actorTarget" Data.Aeson..?= actorTarget
        , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..?= id'
        , Data.Aeson.Key.fromString "targets" Data.Aeson..= targets
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (MutedWord actorTarget expiresAt id' targets value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actorTarget" Data.Aeson..?= actorTarget
        , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..?= id'
        , Data.Aeson.Key.fromString "targets" Data.Aeson..= targets
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

mutedWord'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MutedWord -> kv
mutedWord'AesonFields (MutedWord actorTarget expiresAt id' targets value) =
  mconcat
    [ Data.Aeson.Key.fromString "actorTarget" Data.Aeson..?= actorTarget
    , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
    , Data.Aeson.Key.fromString "id" Data.Aeson..?= id'
    , Data.Aeson.Key.fromString "targets" Data.Aeson..= targets
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

newtype MutedWordTarget = MutedWordTarget
  { getMutedWordTarget :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MutedWordTarget where
  parseJSON = Data.Aeson.withText "MutedWordTarget" $ pure . MutedWordTarget

instance Data.Aeson.ToJSON MutedWordTarget where
  toJSON (MutedWordTarget getMutedWordTarget) = Data.Aeson.toJSON getMutedWordTarget
  toEncoding (MutedWordTarget getMutedWordTarget) = Data.Aeson.toEncoding getMutedWordTarget

data MutedWordsPref = MutedWordsPref
  { items :: [MutedWord]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON MutedWordsPref where
  parseJSON = Data.Aeson.withObject "MutedWordsPref" $ \v -> do
    items <- v Data.Aeson..: Data.Aeson.Key.fromString "items"
    pure $ MutedWordsPref items

instance Data.Aeson.ToJSON MutedWordsPref where
  toJSON (MutedWordsPref items) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]
  toEncoding (MutedWordsPref items) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]

mutedWordsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MutedWordsPref -> kv
mutedWordsPref'AesonFields (MutedWordsPref items) =
  mconcat
    [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
    ]

data Nux = Nux
  { completed :: Bool
  , data' :: Maybe Data.Text.Text
  , expiresAt :: Maybe Data.Text.Text
  , id' :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Nux where
  parseJSON = Data.Aeson.withObject "Nux" $ \v -> do
    completed <- v Data.Aeson..: Data.Aeson.Key.fromString "completed"
    data' <- v Data.Aeson..:? Data.Aeson.Key.fromString "data"
    expiresAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "expiresAt"
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    pure $ Nux completed data' expiresAt id'

instance Data.Aeson.ToJSON Nux where
  toJSON (Nux completed data' expiresAt id') =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "completed" Data.Aeson..= completed
        , Data.Aeson.Key.fromString "data" Data.Aeson..?= data'
        , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        ]
  toEncoding (Nux completed data' expiresAt id') =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "completed" Data.Aeson..= completed
        , Data.Aeson.Key.fromString "data" Data.Aeson..?= data'
        , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
        , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        ]

nux'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Nux -> kv
nux'AesonFields (Nux completed data' expiresAt id') =
  mconcat
    [ Data.Aeson.Key.fromString "completed" Data.Aeson..= completed
    , Data.Aeson.Key.fromString "data" Data.Aeson..?= data'
    , Data.Aeson.Key.fromString "expiresAt" Data.Aeson..?= expiresAt
    , Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    ]

data PersonalDetailsPref = PersonalDetailsPref
  { birthDate :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PersonalDetailsPref where
  parseJSON = Data.Aeson.withObject "PersonalDetailsPref" $ \v -> do
    birthDate <- v Data.Aeson..:? Data.Aeson.Key.fromString "birthDate"
    pure $ PersonalDetailsPref birthDate

instance Data.Aeson.ToJSON PersonalDetailsPref where
  toJSON (PersonalDetailsPref birthDate) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "birthDate" Data.Aeson..?= birthDate
        ]
  toEncoding (PersonalDetailsPref birthDate) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "birthDate" Data.Aeson..?= birthDate
        ]

personalDetailsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => PersonalDetailsPref -> kv
personalDetailsPref'AesonFields (PersonalDetailsPref birthDate) =
  mconcat
    [ Data.Aeson.Key.fromString "birthDate" Data.Aeson..?= birthDate
    ]

newtype Preferences = Preferences
  { getPreferences :: [PreferencesKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Preferences where
  parseJSON = fmap Preferences . Data.Aeson.parseJSON

instance Data.Aeson.ToJSON Preferences where
  toJSON (Preferences getPreferences) = Data.Aeson.toJSON getPreferences
  toEncoding (Preferences getPreferences) = Data.Aeson.toEncoding getPreferences
data ProfileAssociated = ProfileAssociated
  { chat :: Maybe ProfileAssociatedChat
  , feedgens :: Maybe Integer
  , labeler :: Maybe Bool
  , lists :: Maybe Integer
  , starterPacks :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileAssociated where
  parseJSON = Data.Aeson.withObject "ProfileAssociated" $ \v -> do
    chat <- v Data.Aeson..:? Data.Aeson.Key.fromString "chat"
    feedgens <- v Data.Aeson..:? Data.Aeson.Key.fromString "feedgens"
    labeler <- v Data.Aeson..:? Data.Aeson.Key.fromString "labeler"
    lists <- v Data.Aeson..:? Data.Aeson.Key.fromString "lists"
    starterPacks <- v Data.Aeson..:? Data.Aeson.Key.fromString "starterPacks"
    pure $ ProfileAssociated chat feedgens labeler lists starterPacks

instance Data.Aeson.ToJSON ProfileAssociated where
  toJSON (ProfileAssociated chat feedgens labeler lists starterPacks) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
        , Data.Aeson.Key.fromString "feedgens" Data.Aeson..?= feedgens
        , Data.Aeson.Key.fromString "labeler" Data.Aeson..?= labeler
        , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
        , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..?= starterPacks
        ]
  toEncoding (ProfileAssociated chat feedgens labeler lists starterPacks) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
        , Data.Aeson.Key.fromString "feedgens" Data.Aeson..?= feedgens
        , Data.Aeson.Key.fromString "labeler" Data.Aeson..?= labeler
        , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
        , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..?= starterPacks
        ]

profileAssociated'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileAssociated -> kv
profileAssociated'AesonFields (ProfileAssociated chat feedgens labeler lists starterPacks) =
  mconcat
    [ Data.Aeson.Key.fromString "chat" Data.Aeson..?= chat
    , Data.Aeson.Key.fromString "feedgens" Data.Aeson..?= feedgens
    , Data.Aeson.Key.fromString "labeler" Data.Aeson..?= labeler
    , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
    , Data.Aeson.Key.fromString "starterPacks" Data.Aeson..?= starterPacks
    ]

data ProfileAssociatedChat = ProfileAssociatedChat
  { allowIncoming :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileAssociatedChat where
  parseJSON = Data.Aeson.withObject "ProfileAssociatedChat" $ \v -> do
    allowIncoming <- v Data.Aeson..: Data.Aeson.Key.fromString "allowIncoming"
    pure $ ProfileAssociatedChat allowIncoming

instance Data.Aeson.ToJSON ProfileAssociatedChat where
  toJSON (ProfileAssociatedChat allowIncoming) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
        ]
  toEncoding (ProfileAssociatedChat allowIncoming) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
        ]

profileAssociatedChat'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileAssociatedChat -> kv
profileAssociatedChat'AesonFields (ProfileAssociatedChat allowIncoming) =
  mconcat
    [ Data.Aeson.Key.fromString "allowIncoming" Data.Aeson..= allowIncoming
    ]

data ProfileView = ProfileView
  { associated :: Maybe ProfileAssociated
  , avatar :: Maybe Data.Text.Text
  , createdAt :: Maybe Data.Text.Text
  , description :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , displayName :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , indexedAt :: Maybe Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , viewer :: Maybe ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileView where
  parseJSON = Data.Aeson.withObject "ProfileView" $ \v -> do
    associated <- v Data.Aeson..:? Data.Aeson.Key.fromString "associated"
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    createdAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..:? Data.Aeson.Key.fromString "displayName"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    indexedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ProfileView associated avatar createdAt description did displayName handle indexedAt labels viewer

instance Data.Aeson.ToJSON ProfileView where
  toJSON (ProfileView associated avatar createdAt description did displayName handle indexedAt labels viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ProfileView associated avatar createdAt description did displayName handle indexedAt labels viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

profileView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileView -> kv
profileView'AesonFields (ProfileView associated avatar createdAt description did displayName handle indexedAt labels viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data ProfileViewBasic = ProfileViewBasic
  { associated :: Maybe ProfileAssociated
  , avatar :: Maybe Data.Text.Text
  , createdAt :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , displayName :: Maybe Data.Text.Text
  , handle :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , viewer :: Maybe ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileViewBasic where
  parseJSON = Data.Aeson.withObject "ProfileViewBasic" $ \v -> do
    associated <- v Data.Aeson..:? Data.Aeson.Key.fromString "associated"
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    createdAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "createdAt"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..:? Data.Aeson.Key.fromString "displayName"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ProfileViewBasic associated avatar createdAt did displayName handle labels viewer

instance Data.Aeson.ToJSON ProfileViewBasic where
  toJSON (ProfileViewBasic associated avatar createdAt did displayName handle labels viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ProfileViewBasic associated avatar createdAt did displayName handle labels viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

profileViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewBasic -> kv
profileViewBasic'AesonFields (ProfileViewBasic associated avatar createdAt did displayName handle labels viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data ProfileViewDetailed = ProfileViewDetailed
  { associated :: Maybe ProfileAssociated
  , avatar :: Maybe Data.Text.Text
  , banner :: Maybe Data.Text.Text
  , createdAt :: Maybe Data.Text.Text
  , description :: Maybe Data.Text.Text
  , did :: Data.Text.Text
  , displayName :: Maybe Data.Text.Text
  , followersCount :: Maybe Integer
  , followsCount :: Maybe Integer
  , handle :: Data.Text.Text
  , indexedAt :: Maybe Data.Text.Text
  , joinedViaStarterPack :: Maybe App.Bsky.Graph.Defs.StarterPackViewBasic
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , pinnedPost :: Maybe Com.Atproto.Repo.StrongRef.StrongRef
  , postsCount :: Maybe Integer
  , viewer :: Maybe ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ProfileViewDetailed where
  parseJSON = Data.Aeson.withObject "ProfileViewDetailed" $ \v -> do
    associated <- v Data.Aeson..:? Data.Aeson.Key.fromString "associated"
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    banner <- v Data.Aeson..:? Data.Aeson.Key.fromString "banner"
    createdAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "createdAt"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..:? Data.Aeson.Key.fromString "displayName"
    followersCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "followersCount"
    followsCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "followsCount"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    indexedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "indexedAt"
    joinedViaStarterPack <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedViaStarterPack"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    pinnedPost <- v Data.Aeson..:? Data.Aeson.Key.fromString "pinnedPost"
    postsCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "postsCount"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ProfileViewDetailed associated avatar banner createdAt description did displayName followersCount followsCount handle indexedAt joinedViaStarterPack labels pinnedPost postsCount viewer

instance Data.Aeson.ToJSON ProfileViewDetailed where
  toJSON (ProfileViewDetailed associated avatar banner createdAt description did displayName followersCount followsCount handle indexedAt joinedViaStarterPack labels pinnedPost postsCount viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "banner" Data.Aeson..?= banner
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "followersCount" Data.Aeson..?= followersCount
        , Data.Aeson.Key.fromString "followsCount" Data.Aeson..?= followsCount
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
        , Data.Aeson.Key.fromString "postsCount" Data.Aeson..?= postsCount
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ProfileViewDetailed associated avatar banner createdAt description did displayName followersCount followsCount handle indexedAt joinedViaStarterPack labels pinnedPost postsCount viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "banner" Data.Aeson..?= banner
        , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
        , Data.Aeson.Key.fromString "followersCount" Data.Aeson..?= followersCount
        , Data.Aeson.Key.fromString "followsCount" Data.Aeson..?= followsCount
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
        , Data.Aeson.Key.fromString "postsCount" Data.Aeson..?= postsCount
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

profileViewDetailed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewDetailed -> kv
profileViewDetailed'AesonFields (ProfileViewDetailed associated avatar banner createdAt description did displayName followersCount followsCount handle indexedAt joinedViaStarterPack labels pinnedPost postsCount viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "associated" Data.Aeson..?= associated
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "banner" Data.Aeson..?= banner
    , Data.Aeson.Key.fromString "createdAt" Data.Aeson..?= createdAt
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..?= displayName
    , Data.Aeson.Key.fromString "followersCount" Data.Aeson..?= followersCount
    , Data.Aeson.Key.fromString "followsCount" Data.Aeson..?= followsCount
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
    , Data.Aeson.Key.fromString "joinedViaStarterPack" Data.Aeson..?= joinedViaStarterPack
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "pinnedPost" Data.Aeson..?= pinnedPost
    , Data.Aeson.Key.fromString "postsCount" Data.Aeson..?= postsCount
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data SavedFeed = SavedFeed
  { id' :: Data.Text.Text
  , pinned :: Bool
  , type' :: Data.Text.Text
  , value :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SavedFeed where
  parseJSON = Data.Aeson.withObject "SavedFeed" $ \v -> do
    id' <- v Data.Aeson..: Data.Aeson.Key.fromString "id"
    pinned <- v Data.Aeson..: Data.Aeson.Key.fromString "pinned"
    type' <- v Data.Aeson..: Data.Aeson.Key.fromString "type"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ SavedFeed id' pinned type' value

instance Data.Aeson.ToJSON SavedFeed where
  toJSON (SavedFeed id' pinned type' value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
        , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (SavedFeed id' pinned type' value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
        , Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
        , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

savedFeed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeed -> kv
savedFeed'AesonFields (SavedFeed id' pinned type' value) =
  mconcat
    [ Data.Aeson.Key.fromString "id" Data.Aeson..= id'
    , Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
    , Data.Aeson.Key.fromString "type" Data.Aeson..= type'
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data SavedFeedsPref = SavedFeedsPref
  { pinned :: [Data.Text.Text]
  , saved :: [Data.Text.Text]
  , timelineIndex :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SavedFeedsPref where
  parseJSON = Data.Aeson.withObject "SavedFeedsPref" $ \v -> do
    pinned <- v Data.Aeson..: Data.Aeson.Key.fromString "pinned"
    saved <- v Data.Aeson..: Data.Aeson.Key.fromString "saved"
    timelineIndex <- v Data.Aeson..:? Data.Aeson.Key.fromString "timelineIndex"
    pure $ SavedFeedsPref pinned saved timelineIndex

instance Data.Aeson.ToJSON SavedFeedsPref where
  toJSON (SavedFeedsPref pinned saved timelineIndex) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
        , Data.Aeson.Key.fromString "saved" Data.Aeson..= saved
        , Data.Aeson.Key.fromString "timelineIndex" Data.Aeson..?= timelineIndex
        ]
  toEncoding (SavedFeedsPref pinned saved timelineIndex) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
        , Data.Aeson.Key.fromString "saved" Data.Aeson..= saved
        , Data.Aeson.Key.fromString "timelineIndex" Data.Aeson..?= timelineIndex
        ]

savedFeedsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeedsPref -> kv
savedFeedsPref'AesonFields (SavedFeedsPref pinned saved timelineIndex) =
  mconcat
    [ Data.Aeson.Key.fromString "pinned" Data.Aeson..= pinned
    , Data.Aeson.Key.fromString "saved" Data.Aeson..= saved
    , Data.Aeson.Key.fromString "timelineIndex" Data.Aeson..?= timelineIndex
    ]

data SavedFeedsPrefV2 = SavedFeedsPrefV2
  { items :: [SavedFeed]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SavedFeedsPrefV2 where
  parseJSON = Data.Aeson.withObject "SavedFeedsPrefV2" $ \v -> do
    items <- v Data.Aeson..: Data.Aeson.Key.fromString "items"
    pure $ SavedFeedsPrefV2 items

instance Data.Aeson.ToJSON SavedFeedsPrefV2 where
  toJSON (SavedFeedsPrefV2 items) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]
  toEncoding (SavedFeedsPrefV2 items) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
        ]

savedFeedsPrefV2'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeedsPrefV2 -> kv
savedFeedsPrefV2'AesonFields (SavedFeedsPrefV2 items) =
  mconcat
    [ Data.Aeson.Key.fromString "items" Data.Aeson..= items
    ]

data ThreadViewPref = ThreadViewPref
  { prioritizeFollowedUsers :: Maybe Bool
  , sort :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadViewPref where
  parseJSON = Data.Aeson.withObject "ThreadViewPref" $ \v -> do
    prioritizeFollowedUsers <- v Data.Aeson..:? Data.Aeson.Key.fromString "prioritizeFollowedUsers"
    sort <- v Data.Aeson..:? Data.Aeson.Key.fromString "sort"
    pure $ ThreadViewPref prioritizeFollowedUsers sort

instance Data.Aeson.ToJSON ThreadViewPref where
  toJSON (ThreadViewPref prioritizeFollowedUsers sort) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "prioritizeFollowedUsers" Data.Aeson..?= prioritizeFollowedUsers
        , Data.Aeson.Key.fromString "sort" Data.Aeson..?= sort
        ]
  toEncoding (ThreadViewPref prioritizeFollowedUsers sort) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "prioritizeFollowedUsers" Data.Aeson..?= prioritizeFollowedUsers
        , Data.Aeson.Key.fromString "sort" Data.Aeson..?= sort
        ]

threadViewPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadViewPref -> kv
threadViewPref'AesonFields (ThreadViewPref prioritizeFollowedUsers sort) =
  mconcat
    [ Data.Aeson.Key.fromString "prioritizeFollowedUsers" Data.Aeson..?= prioritizeFollowedUsers
    , Data.Aeson.Key.fromString "sort" Data.Aeson..?= sort
    ]

data ViewerState = ViewerState
  { blockedBy :: Maybe Bool
  , blocking :: Maybe Data.Text.Text
  , blockingByList :: Maybe App.Bsky.Graph.Defs.ListViewBasic
  , followedBy :: Maybe Data.Text.Text
  , following :: Maybe Data.Text.Text
  , knownFollowers :: Maybe KnownFollowers
  , muted :: Maybe Bool
  , mutedByList :: Maybe App.Bsky.Graph.Defs.ListViewBasic
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewerState where
  parseJSON = Data.Aeson.withObject "ViewerState" $ \v -> do
    blockedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "blockedBy"
    blocking <- v Data.Aeson..:? Data.Aeson.Key.fromString "blocking"
    blockingByList <- v Data.Aeson..:? Data.Aeson.Key.fromString "blockingByList"
    followedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "followedBy"
    following <- v Data.Aeson..:? Data.Aeson.Key.fromString "following"
    knownFollowers <- v Data.Aeson..:? Data.Aeson.Key.fromString "knownFollowers"
    muted <- v Data.Aeson..:? Data.Aeson.Key.fromString "muted"
    mutedByList <- v Data.Aeson..:? Data.Aeson.Key.fromString "mutedByList"
    pure $ ViewerState blockedBy blocking blockingByList followedBy following knownFollowers muted mutedByList

instance Data.Aeson.ToJSON ViewerState where
  toJSON (ViewerState blockedBy blocking blockingByList followedBy following knownFollowers muted mutedByList) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blockedBy" Data.Aeson..?= blockedBy
        , Data.Aeson.Key.fromString "blocking" Data.Aeson..?= blocking
        , Data.Aeson.Key.fromString "blockingByList" Data.Aeson..?= blockingByList
        , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
        , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
        , Data.Aeson.Key.fromString "knownFollowers" Data.Aeson..?= knownFollowers
        , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
        , Data.Aeson.Key.fromString "mutedByList" Data.Aeson..?= mutedByList
        ]
  toEncoding (ViewerState blockedBy blocking blockingByList followedBy following knownFollowers muted mutedByList) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blockedBy" Data.Aeson..?= blockedBy
        , Data.Aeson.Key.fromString "blocking" Data.Aeson..?= blocking
        , Data.Aeson.Key.fromString "blockingByList" Data.Aeson..?= blockingByList
        , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
        , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
        , Data.Aeson.Key.fromString "knownFollowers" Data.Aeson..?= knownFollowers
        , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
        , Data.Aeson.Key.fromString "mutedByList" Data.Aeson..?= mutedByList
        ]

viewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerState -> kv
viewerState'AesonFields (ViewerState blockedBy blocking blockingByList followedBy following knownFollowers muted mutedByList) =
  mconcat
    [ Data.Aeson.Key.fromString "blockedBy" Data.Aeson..?= blockedBy
    , Data.Aeson.Key.fromString "blocking" Data.Aeson..?= blocking
    , Data.Aeson.Key.fromString "blockingByList" Data.Aeson..?= blockingByList
    , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
    , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
    , Data.Aeson.Key.fromString "knownFollowers" Data.Aeson..?= knownFollowers
    , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
    , Data.Aeson.Key.fromString "mutedByList" Data.Aeson..?= mutedByList
    ]

data PreferencesKind
  = PreferencesKindAdultContentPref AdultContentPref
  | PreferencesKindContentLabelPref ContentLabelPref
  | PreferencesKindSavedFeedsPref SavedFeedsPref
  | PreferencesKindSavedFeedsPrefV2 SavedFeedsPrefV2
  | PreferencesKindPersonalDetailsPref PersonalDetailsPref
  | PreferencesKindFeedViewPref FeedViewPref
  | PreferencesKindThreadViewPref ThreadViewPref
  | PreferencesKindInterestsPref InterestsPref
  | PreferencesKindMutedWordsPref MutedWordsPref
  | PreferencesKindHiddenPostsPref HiddenPostsPref
  | PreferencesKindBskyAppStatePref BskyAppStatePref
  | PreferencesKindLabelersPref LabelersPref
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PreferencesKind where
  parseJSON = Data.Aeson.withObject "PreferencesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.actor.defs#adultContentPref" -> PreferencesKindAdultContentPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#contentLabelPref" -> PreferencesKindContentLabelPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#savedFeedsPref" -> PreferencesKindSavedFeedsPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#savedFeedsPrefV2" -> PreferencesKindSavedFeedsPrefV2 <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#personalDetailsPref" -> PreferencesKindPersonalDetailsPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#feedViewPref" -> PreferencesKindFeedViewPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#threadViewPref" -> PreferencesKindThreadViewPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#interestsPref" -> PreferencesKindInterestsPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#mutedWordsPref" -> PreferencesKindMutedWordsPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#hiddenPostsPref" -> PreferencesKindHiddenPostsPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#bskyAppStatePref" -> PreferencesKindBskyAppStatePref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.actor.defs#labelersPref" -> PreferencesKindLabelersPref <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON PreferencesKind where
  toJSON = \case
    PreferencesKindAdultContentPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#adultContentPref") <> (App.Bsky.Actor.Defs.adultContentPref'AesonFields v))
    PreferencesKindContentLabelPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#contentLabelPref") <> (App.Bsky.Actor.Defs.contentLabelPref'AesonFields v))
    PreferencesKindSavedFeedsPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#savedFeedsPref") <> (App.Bsky.Actor.Defs.savedFeedsPref'AesonFields v))
    PreferencesKindSavedFeedsPrefV2 v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#savedFeedsPrefV2") <> (App.Bsky.Actor.Defs.savedFeedsPrefV2'AesonFields v))
    PreferencesKindPersonalDetailsPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#personalDetailsPref") <> (App.Bsky.Actor.Defs.personalDetailsPref'AesonFields v))
    PreferencesKindFeedViewPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#feedViewPref") <> (App.Bsky.Actor.Defs.feedViewPref'AesonFields v))
    PreferencesKindThreadViewPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#threadViewPref") <> (App.Bsky.Actor.Defs.threadViewPref'AesonFields v))
    PreferencesKindInterestsPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#interestsPref") <> (App.Bsky.Actor.Defs.interestsPref'AesonFields v))
    PreferencesKindMutedWordsPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#mutedWordsPref") <> (App.Bsky.Actor.Defs.mutedWordsPref'AesonFields v))
    PreferencesKindHiddenPostsPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#hiddenPostsPref") <> (App.Bsky.Actor.Defs.hiddenPostsPref'AesonFields v))
    PreferencesKindBskyAppStatePref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#bskyAppStatePref") <> (App.Bsky.Actor.Defs.bskyAppStatePref'AesonFields v))
    PreferencesKindLabelersPref v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#labelersPref") <> (App.Bsky.Actor.Defs.labelersPref'AesonFields v))
  toEncoding = \case
    PreferencesKindAdultContentPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#adultContentPref") <> (App.Bsky.Actor.Defs.adultContentPref'AesonFields v))
    PreferencesKindContentLabelPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#contentLabelPref") <> (App.Bsky.Actor.Defs.contentLabelPref'AesonFields v))
    PreferencesKindSavedFeedsPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#savedFeedsPref") <> (App.Bsky.Actor.Defs.savedFeedsPref'AesonFields v))
    PreferencesKindSavedFeedsPrefV2 v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#savedFeedsPrefV2") <> (App.Bsky.Actor.Defs.savedFeedsPrefV2'AesonFields v))
    PreferencesKindPersonalDetailsPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#personalDetailsPref") <> (App.Bsky.Actor.Defs.personalDetailsPref'AesonFields v))
    PreferencesKindFeedViewPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#feedViewPref") <> (App.Bsky.Actor.Defs.feedViewPref'AesonFields v))
    PreferencesKindThreadViewPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#threadViewPref") <> (App.Bsky.Actor.Defs.threadViewPref'AesonFields v))
    PreferencesKindInterestsPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#interestsPref") <> (App.Bsky.Actor.Defs.interestsPref'AesonFields v))
    PreferencesKindMutedWordsPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#mutedWordsPref") <> (App.Bsky.Actor.Defs.mutedWordsPref'AesonFields v))
    PreferencesKindHiddenPostsPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#hiddenPostsPref") <> (App.Bsky.Actor.Defs.hiddenPostsPref'AesonFields v))
    PreferencesKindBskyAppStatePref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#bskyAppStatePref") <> (App.Bsky.Actor.Defs.bskyAppStatePref'AesonFields v))
    PreferencesKindLabelersPref v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.actor.defs#labelersPref") <> (App.Bsky.Actor.Defs.labelersPref'AesonFields v))
