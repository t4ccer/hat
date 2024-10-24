module App.Bsky.Feed.Defs where

import qualified Data.Aeson

data BlockedAuthor

instance Show BlockedAuthor
instance Read BlockedAuthor
instance Eq BlockedAuthor
instance Ord BlockedAuthor
instance Data.Aeson.FromJSON BlockedAuthor

instance Data.Aeson.ToJSON BlockedAuthor

blockedAuthor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlockedAuthor -> kv
data BlockedPost

instance Show BlockedPost
instance Read BlockedPost
instance Eq BlockedPost
instance Ord BlockedPost
instance Data.Aeson.FromJSON BlockedPost

instance Data.Aeson.ToJSON BlockedPost

blockedPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlockedPost -> kv
data FeedViewPost

instance Show FeedViewPost
instance Read FeedViewPost
instance Eq FeedViewPost
instance Ord FeedViewPost
instance Data.Aeson.FromJSON FeedViewPost

instance Data.Aeson.ToJSON FeedViewPost

feedViewPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedViewPost -> kv
data GeneratorView

instance Show GeneratorView
instance Read GeneratorView
instance Eq GeneratorView
instance Ord GeneratorView
instance Data.Aeson.FromJSON GeneratorView

instance Data.Aeson.ToJSON GeneratorView

generatorView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GeneratorView -> kv
data GeneratorViewerState

instance Show GeneratorViewerState
instance Read GeneratorViewerState
instance Eq GeneratorViewerState
instance Ord GeneratorViewerState
instance Data.Aeson.FromJSON GeneratorViewerState

instance Data.Aeson.ToJSON GeneratorViewerState

generatorViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GeneratorViewerState -> kv
data Interaction

instance Show Interaction
instance Read Interaction
instance Eq Interaction
instance Ord Interaction
instance Data.Aeson.FromJSON Interaction

instance Data.Aeson.ToJSON Interaction

interaction'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Interaction -> kv
data NotFoundPost

instance Show NotFoundPost
instance Read NotFoundPost
instance Eq NotFoundPost
instance Ord NotFoundPost
instance Data.Aeson.FromJSON NotFoundPost

instance Data.Aeson.ToJSON NotFoundPost

notFoundPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => NotFoundPost -> kv
data PostView

instance Show PostView
instance Read PostView
instance Eq PostView
instance Ord PostView
instance Data.Aeson.FromJSON PostView

instance Data.Aeson.ToJSON PostView

postView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => PostView -> kv
data ReasonPin

instance Show ReasonPin
instance Read ReasonPin
instance Eq ReasonPin
instance Ord ReasonPin
instance Data.Aeson.FromJSON ReasonPin

instance Data.Aeson.ToJSON ReasonPin

reasonPin'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReasonPin -> kv
data ReasonRepost

instance Show ReasonRepost
instance Read ReasonRepost
instance Eq ReasonRepost
instance Ord ReasonRepost
instance Data.Aeson.FromJSON ReasonRepost

instance Data.Aeson.ToJSON ReasonRepost

reasonRepost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReasonRepost -> kv
data ReplyRef

instance Show ReplyRef
instance Read ReplyRef
instance Eq ReplyRef
instance Ord ReplyRef
instance Data.Aeson.FromJSON ReplyRef

instance Data.Aeson.ToJSON ReplyRef

replyRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReplyRef -> kv
data SkeletonFeedPost

instance Show SkeletonFeedPost
instance Read SkeletonFeedPost
instance Eq SkeletonFeedPost
instance Ord SkeletonFeedPost
instance Data.Aeson.FromJSON SkeletonFeedPost

instance Data.Aeson.ToJSON SkeletonFeedPost

skeletonFeedPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonFeedPost -> kv
data SkeletonReasonPin

instance Show SkeletonReasonPin
instance Read SkeletonReasonPin
instance Eq SkeletonReasonPin
instance Ord SkeletonReasonPin
instance Data.Aeson.FromJSON SkeletonReasonPin

instance Data.Aeson.ToJSON SkeletonReasonPin

skeletonReasonPin'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonReasonPin -> kv
data SkeletonReasonRepost

instance Show SkeletonReasonRepost
instance Read SkeletonReasonRepost
instance Eq SkeletonReasonRepost
instance Ord SkeletonReasonRepost
instance Data.Aeson.FromJSON SkeletonReasonRepost

instance Data.Aeson.ToJSON SkeletonReasonRepost

skeletonReasonRepost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonReasonRepost -> kv
data ThreadViewPost

instance Show ThreadViewPost
instance Read ThreadViewPost
instance Eq ThreadViewPost
instance Ord ThreadViewPost
instance Data.Aeson.FromJSON ThreadViewPost

instance Data.Aeson.ToJSON ThreadViewPost

threadViewPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadViewPost -> kv
data ThreadgateView

instance Show ThreadgateView
instance Read ThreadgateView
instance Eq ThreadgateView
instance Ord ThreadgateView
instance Data.Aeson.FromJSON ThreadgateView

instance Data.Aeson.ToJSON ThreadgateView

threadgateView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadgateView -> kv
data ViewerState

instance Show ViewerState
instance Read ViewerState
instance Eq ViewerState
instance Ord ViewerState
instance Data.Aeson.FromJSON ViewerState

instance Data.Aeson.ToJSON ViewerState

viewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerState -> kv
data FeedViewPostReasonKind

instance Show FeedViewPostReasonKind
instance Read FeedViewPostReasonKind
instance Eq FeedViewPostReasonKind
instance Ord FeedViewPostReasonKind
instance Data.Aeson.FromJSON FeedViewPostReasonKind

instance Data.Aeson.ToJSON FeedViewPostReasonKind

data PostViewEmbedKind

instance Show PostViewEmbedKind
instance Read PostViewEmbedKind
instance Eq PostViewEmbedKind
instance Ord PostViewEmbedKind
instance Data.Aeson.FromJSON PostViewEmbedKind

instance Data.Aeson.ToJSON PostViewEmbedKind

data ReplyRefParentKind

instance Show ReplyRefParentKind
instance Read ReplyRefParentKind
instance Eq ReplyRefParentKind
instance Ord ReplyRefParentKind
instance Data.Aeson.FromJSON ReplyRefParentKind

instance Data.Aeson.ToJSON ReplyRefParentKind

data ReplyRefRootKind

instance Show ReplyRefRootKind
instance Read ReplyRefRootKind
instance Eq ReplyRefRootKind
instance Ord ReplyRefRootKind
instance Data.Aeson.FromJSON ReplyRefRootKind

instance Data.Aeson.ToJSON ReplyRefRootKind

data SkeletonFeedPostReasonKind

instance Show SkeletonFeedPostReasonKind
instance Read SkeletonFeedPostReasonKind
instance Eq SkeletonFeedPostReasonKind
instance Ord SkeletonFeedPostReasonKind
instance Data.Aeson.FromJSON SkeletonFeedPostReasonKind

instance Data.Aeson.ToJSON SkeletonFeedPostReasonKind

data ThreadViewPostParentKind

instance Show ThreadViewPostParentKind
instance Read ThreadViewPostParentKind
instance Eq ThreadViewPostParentKind
instance Ord ThreadViewPostParentKind
instance Data.Aeson.FromJSON ThreadViewPostParentKind

instance Data.Aeson.ToJSON ThreadViewPostParentKind

data ThreadViewPostRepliesKind

instance Show ThreadViewPostRepliesKind
instance Read ThreadViewPostRepliesKind
instance Eq ThreadViewPostRepliesKind
instance Ord ThreadViewPostRepliesKind
instance Data.Aeson.FromJSON ThreadViewPostRepliesKind

instance Data.Aeson.ToJSON ThreadViewPostRepliesKind
