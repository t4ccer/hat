module App.Bsky.Actor.Defs where

import qualified Data.Aeson

data AdultContentPref

instance Show AdultContentPref
instance Read AdultContentPref
instance Eq AdultContentPref
instance Ord AdultContentPref
instance Data.Aeson.FromJSON AdultContentPref

instance Data.Aeson.ToJSON AdultContentPref

adultContentPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => AdultContentPref -> kv
data BskyAppProgressGuide

instance Show BskyAppProgressGuide
instance Read BskyAppProgressGuide
instance Eq BskyAppProgressGuide
instance Ord BskyAppProgressGuide
instance Data.Aeson.FromJSON BskyAppProgressGuide

instance Data.Aeson.ToJSON BskyAppProgressGuide

bskyAppProgressGuide'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BskyAppProgressGuide -> kv
data BskyAppStatePref

instance Show BskyAppStatePref
instance Read BskyAppStatePref
instance Eq BskyAppStatePref
instance Ord BskyAppStatePref
instance Data.Aeson.FromJSON BskyAppStatePref

instance Data.Aeson.ToJSON BskyAppStatePref

bskyAppStatePref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BskyAppStatePref -> kv
data ContentLabelPref

instance Show ContentLabelPref
instance Read ContentLabelPref
instance Eq ContentLabelPref
instance Ord ContentLabelPref
instance Data.Aeson.FromJSON ContentLabelPref

instance Data.Aeson.ToJSON ContentLabelPref

contentLabelPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ContentLabelPref -> kv
data FeedViewPref

instance Show FeedViewPref
instance Read FeedViewPref
instance Eq FeedViewPref
instance Ord FeedViewPref
instance Data.Aeson.FromJSON FeedViewPref

instance Data.Aeson.ToJSON FeedViewPref

feedViewPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedViewPref -> kv
data HiddenPostsPref

instance Show HiddenPostsPref
instance Read HiddenPostsPref
instance Eq HiddenPostsPref
instance Ord HiddenPostsPref
instance Data.Aeson.FromJSON HiddenPostsPref

instance Data.Aeson.ToJSON HiddenPostsPref

hiddenPostsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => HiddenPostsPref -> kv
data InterestsPref

instance Show InterestsPref
instance Read InterestsPref
instance Eq InterestsPref
instance Ord InterestsPref
instance Data.Aeson.FromJSON InterestsPref

instance Data.Aeson.ToJSON InterestsPref

interestsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => InterestsPref -> kv
data KnownFollowers

instance Show KnownFollowers
instance Read KnownFollowers
instance Eq KnownFollowers
instance Ord KnownFollowers
instance Data.Aeson.FromJSON KnownFollowers

instance Data.Aeson.ToJSON KnownFollowers

knownFollowers'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => KnownFollowers -> kv
data LabelerPrefItem

instance Show LabelerPrefItem
instance Read LabelerPrefItem
instance Eq LabelerPrefItem
instance Ord LabelerPrefItem
instance Data.Aeson.FromJSON LabelerPrefItem

instance Data.Aeson.ToJSON LabelerPrefItem

labelerPrefItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelerPrefItem -> kv
data LabelersPref

instance Show LabelersPref
instance Read LabelersPref
instance Eq LabelersPref
instance Ord LabelersPref
instance Data.Aeson.FromJSON LabelersPref

instance Data.Aeson.ToJSON LabelersPref

labelersPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LabelersPref -> kv
data MutedWord

instance Show MutedWord
instance Read MutedWord
instance Eq MutedWord
instance Ord MutedWord
instance Data.Aeson.FromJSON MutedWord

instance Data.Aeson.ToJSON MutedWord

mutedWord'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MutedWord -> kv
data MutedWordTarget

instance Show MutedWordTarget
instance Read MutedWordTarget
instance Eq MutedWordTarget
instance Ord MutedWordTarget
instance Data.Aeson.FromJSON MutedWordTarget

instance Data.Aeson.ToJSON MutedWordTarget

data MutedWordsPref

instance Show MutedWordsPref
instance Read MutedWordsPref
instance Eq MutedWordsPref
instance Ord MutedWordsPref
instance Data.Aeson.FromJSON MutedWordsPref

instance Data.Aeson.ToJSON MutedWordsPref

mutedWordsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MutedWordsPref -> kv
data Nux

instance Show Nux
instance Read Nux
instance Eq Nux
instance Ord Nux
instance Data.Aeson.FromJSON Nux

instance Data.Aeson.ToJSON Nux

nux'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Nux -> kv
data PersonalDetailsPref

instance Show PersonalDetailsPref
instance Read PersonalDetailsPref
instance Eq PersonalDetailsPref
instance Ord PersonalDetailsPref
instance Data.Aeson.FromJSON PersonalDetailsPref

instance Data.Aeson.ToJSON PersonalDetailsPref

personalDetailsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => PersonalDetailsPref -> kv
data Preferences

instance Show Preferences
instance Read Preferences
instance Eq Preferences
instance Ord Preferences
instance Data.Aeson.FromJSON Preferences

instance Data.Aeson.ToJSON Preferences

data ProfileAssociated

instance Show ProfileAssociated
instance Read ProfileAssociated
instance Eq ProfileAssociated
instance Ord ProfileAssociated
instance Data.Aeson.FromJSON ProfileAssociated

instance Data.Aeson.ToJSON ProfileAssociated

profileAssociated'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileAssociated -> kv
data ProfileAssociatedChat

instance Show ProfileAssociatedChat
instance Read ProfileAssociatedChat
instance Eq ProfileAssociatedChat
instance Ord ProfileAssociatedChat
instance Data.Aeson.FromJSON ProfileAssociatedChat

instance Data.Aeson.ToJSON ProfileAssociatedChat

profileAssociatedChat'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileAssociatedChat -> kv
data ProfileView

instance Show ProfileView
instance Read ProfileView
instance Eq ProfileView
instance Ord ProfileView
instance Data.Aeson.FromJSON ProfileView

instance Data.Aeson.ToJSON ProfileView

profileView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileView -> kv
data ProfileViewBasic

instance Show ProfileViewBasic
instance Read ProfileViewBasic
instance Eq ProfileViewBasic
instance Ord ProfileViewBasic
instance Data.Aeson.FromJSON ProfileViewBasic

instance Data.Aeson.ToJSON ProfileViewBasic

profileViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewBasic -> kv
data ProfileViewDetailed

instance Show ProfileViewDetailed
instance Read ProfileViewDetailed
instance Eq ProfileViewDetailed
instance Ord ProfileViewDetailed
instance Data.Aeson.FromJSON ProfileViewDetailed

instance Data.Aeson.ToJSON ProfileViewDetailed

profileViewDetailed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ProfileViewDetailed -> kv
data SavedFeed

instance Show SavedFeed
instance Read SavedFeed
instance Eq SavedFeed
instance Ord SavedFeed
instance Data.Aeson.FromJSON SavedFeed

instance Data.Aeson.ToJSON SavedFeed

savedFeed'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeed -> kv
data SavedFeedsPref

instance Show SavedFeedsPref
instance Read SavedFeedsPref
instance Eq SavedFeedsPref
instance Ord SavedFeedsPref
instance Data.Aeson.FromJSON SavedFeedsPref

instance Data.Aeson.ToJSON SavedFeedsPref

savedFeedsPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeedsPref -> kv
data SavedFeedsPrefV2

instance Show SavedFeedsPrefV2
instance Read SavedFeedsPrefV2
instance Eq SavedFeedsPrefV2
instance Ord SavedFeedsPrefV2
instance Data.Aeson.FromJSON SavedFeedsPrefV2

instance Data.Aeson.ToJSON SavedFeedsPrefV2

savedFeedsPrefV2'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SavedFeedsPrefV2 -> kv
data ThreadViewPref

instance Show ThreadViewPref
instance Read ThreadViewPref
instance Eq ThreadViewPref
instance Ord ThreadViewPref
instance Data.Aeson.FromJSON ThreadViewPref

instance Data.Aeson.ToJSON ThreadViewPref

threadViewPref'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadViewPref -> kv
data ViewerState

instance Show ViewerState
instance Read ViewerState
instance Eq ViewerState
instance Ord ViewerState
instance Data.Aeson.FromJSON ViewerState

instance Data.Aeson.ToJSON ViewerState

viewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerState -> kv
data PreferencesKind

instance Show PreferencesKind
instance Read PreferencesKind
instance Eq PreferencesKind
instance Ord PreferencesKind
instance Data.Aeson.FromJSON PreferencesKind

instance Data.Aeson.ToJSON PreferencesKind
