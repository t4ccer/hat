{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Feed.Defs where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified App.Bsky.Embed.External
import {-# SOURCE #-} qualified App.Bsky.Embed.Images
import {-# SOURCE #-} qualified App.Bsky.Embed.Record
import {-# SOURCE #-} qualified App.Bsky.Embed.RecordWithMedia
import {-# SOURCE #-} qualified App.Bsky.Embed.Video
import {-# SOURCE #-} qualified App.Bsky.Graph.Defs
import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data BlockedAuthor = BlockedAuthor
  { did :: Data.Text.Text
  , viewer :: Maybe App.Bsky.Actor.Defs.ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BlockedAuthor where
  parseJSON = Data.Aeson.withObject "BlockedAuthor" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ BlockedAuthor did viewer

instance Data.Aeson.ToJSON BlockedAuthor where
  toJSON (BlockedAuthor did viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (BlockedAuthor did viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

blockedAuthor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlockedAuthor -> kv
blockedAuthor'AesonFields (BlockedAuthor did viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data BlockedPost = BlockedPost
  { author :: BlockedAuthor
  , blocked :: Bool
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON BlockedPost where
  parseJSON = Data.Aeson.withObject "BlockedPost" $ \v -> do
    author <- v Data.Aeson..: Data.Aeson.Key.fromString "author"
    blocked <- v Data.Aeson..: Data.Aeson.Key.fromString "blocked"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ BlockedPost author blocked uri

instance Data.Aeson.ToJSON BlockedPost where
  toJSON (BlockedPost author blocked uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (BlockedPost author blocked uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

blockedPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BlockedPost -> kv
blockedPost'AesonFields (BlockedPost author blocked uri) =
  mconcat
    [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
    , Data.Aeson.Key.fromString "blocked" Data.Aeson..= blocked
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data FeedViewPost = FeedViewPost
  { feedContext :: Maybe Data.Text.Text
  , post :: PostView
  , reason :: Maybe FeedViewPostReasonKind
  , reply :: Maybe ReplyRef
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FeedViewPost where
  parseJSON = Data.Aeson.withObject "FeedViewPost" $ \v -> do
    feedContext <- v Data.Aeson..:? Data.Aeson.Key.fromString "feedContext"
    post <- v Data.Aeson..: Data.Aeson.Key.fromString "post"
    reason <- v Data.Aeson..:? Data.Aeson.Key.fromString "reason"
    reply <- v Data.Aeson..:? Data.Aeson.Key.fromString "reply"
    pure $ FeedViewPost feedContext post reason reply

instance Data.Aeson.ToJSON FeedViewPost where
  toJSON (FeedViewPost feedContext post reason reply) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
        , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
        ]
  toEncoding (FeedViewPost feedContext post reason reply) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
        , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
        ]

feedViewPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => FeedViewPost -> kv
feedViewPost'AesonFields (FeedViewPost feedContext post reason reply) =
  mconcat
    [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
    , Data.Aeson.Key.fromString "post" Data.Aeson..= post
    , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
    , Data.Aeson.Key.fromString "reply" Data.Aeson..?= reply
    ]

data GeneratorView = GeneratorView
  { acceptsInteractions :: Maybe Bool
  , avatar :: Maybe Data.Text.Text
  , cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileView
  , description :: Maybe Data.Text.Text
  , descriptionFacets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , did :: Data.Text.Text
  , displayName :: Data.Text.Text
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , likeCount :: Maybe Integer
  , uri :: Data.Text.Text
  , viewer :: Maybe GeneratorViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GeneratorView where
  parseJSON = Data.Aeson.withObject "GeneratorView" $ \v -> do
    acceptsInteractions <- v Data.Aeson..:? Data.Aeson.Key.fromString "acceptsInteractions"
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    descriptionFacets <- v Data.Aeson..:? Data.Aeson.Key.fromString "descriptionFacets"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    displayName <- v Data.Aeson..: Data.Aeson.Key.fromString "displayName"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    likeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "likeCount"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ GeneratorView acceptsInteractions avatar cid creator description descriptionFacets did displayName indexedAt labels likeCount uri viewer

instance Data.Aeson.ToJSON GeneratorView where
  toJSON (GeneratorView acceptsInteractions avatar cid creator description descriptionFacets did displayName indexedAt labels likeCount uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (GeneratorView acceptsInteractions avatar cid creator description descriptionFacets did displayName indexedAt labels likeCount uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
        , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

generatorView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GeneratorView -> kv
generatorView'AesonFields (GeneratorView acceptsInteractions avatar cid creator description descriptionFacets did displayName indexedAt labels likeCount uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "acceptsInteractions" Data.Aeson..?= acceptsInteractions
    , Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "displayName" Data.Aeson..= displayName
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data GeneratorViewerState = GeneratorViewerState
  { like :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GeneratorViewerState where
  parseJSON = Data.Aeson.withObject "GeneratorViewerState" $ \v -> do
    like <- v Data.Aeson..:? Data.Aeson.Key.fromString "like"
    pure $ GeneratorViewerState like

instance Data.Aeson.ToJSON GeneratorViewerState where
  toJSON (GeneratorViewerState like) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        ]
  toEncoding (GeneratorViewerState like) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        ]

generatorViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GeneratorViewerState -> kv
generatorViewerState'AesonFields (GeneratorViewerState like) =
  mconcat
    [ Data.Aeson.Key.fromString "like" Data.Aeson..?= like
    ]

data Interaction = Interaction
  { event :: Maybe Data.Text.Text
  , feedContext :: Maybe Data.Text.Text
  , item :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Interaction where
  parseJSON = Data.Aeson.withObject "Interaction" $ \v -> do
    event <- v Data.Aeson..:? Data.Aeson.Key.fromString "event"
    feedContext <- v Data.Aeson..:? Data.Aeson.Key.fromString "feedContext"
    item <- v Data.Aeson..:? Data.Aeson.Key.fromString "item"
    pure $ Interaction event feedContext item

instance Data.Aeson.ToJSON Interaction where
  toJSON (Interaction event feedContext item) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "event" Data.Aeson..?= event
        , Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "item" Data.Aeson..?= item
        ]
  toEncoding (Interaction event feedContext item) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "event" Data.Aeson..?= event
        , Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "item" Data.Aeson..?= item
        ]

interaction'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Interaction -> kv
interaction'AesonFields (Interaction event feedContext item) =
  mconcat
    [ Data.Aeson.Key.fromString "event" Data.Aeson..?= event
    , Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
    , Data.Aeson.Key.fromString "item" Data.Aeson..?= item
    ]

data NotFoundPost = NotFoundPost
  { notFound :: Bool
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON NotFoundPost where
  parseJSON = Data.Aeson.withObject "NotFoundPost" $ \v -> do
    notFound <- v Data.Aeson..: Data.Aeson.Key.fromString "notFound"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ NotFoundPost notFound uri

instance Data.Aeson.ToJSON NotFoundPost where
  toJSON (NotFoundPost notFound uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (NotFoundPost notFound uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

notFoundPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => NotFoundPost -> kv
notFoundPost'AesonFields (NotFoundPost notFound uri) =
  mconcat
    [ Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data PostView = PostView
  { author :: App.Bsky.Actor.Defs.ProfileViewBasic
  , cid :: Data.Text.Text
  , embed :: Maybe PostViewEmbedKind
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , likeCount :: Maybe Integer
  , quoteCount :: Maybe Integer
  , record :: Data.Aeson.Value
  , replyCount :: Maybe Integer
  , repostCount :: Maybe Integer
  , threadgate :: Maybe ThreadgateView
  , uri :: Data.Text.Text
  , viewer :: Maybe ViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PostView where
  parseJSON = Data.Aeson.withObject "PostView" $ \v -> do
    author <- v Data.Aeson..: Data.Aeson.Key.fromString "author"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    embed <- v Data.Aeson..:? Data.Aeson.Key.fromString "embed"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    likeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "likeCount"
    quoteCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "quoteCount"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    replyCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "replyCount"
    repostCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "repostCount"
    threadgate <- v Data.Aeson..:? Data.Aeson.Key.fromString "threadgate"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ PostView author cid embed indexedAt labels likeCount quoteCount record replyCount repostCount threadgate uri viewer

instance Data.Aeson.ToJSON PostView where
  toJSON (PostView author cid embed indexedAt labels likeCount quoteCount record replyCount repostCount threadgate uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
        , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
        , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (PostView author cid embed indexedAt labels likeCount quoteCount record replyCount repostCount threadgate uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
        , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
        , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
        , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

postView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => PostView -> kv
postView'AesonFields (PostView author cid embed indexedAt labels likeCount quoteCount record replyCount repostCount threadgate uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "author" Data.Aeson..= author
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "embed" Data.Aeson..?= embed
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "likeCount" Data.Aeson..?= likeCount
    , Data.Aeson.Key.fromString "quoteCount" Data.Aeson..?= quoteCount
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    , Data.Aeson.Key.fromString "replyCount" Data.Aeson..?= replyCount
    , Data.Aeson.Key.fromString "repostCount" Data.Aeson..?= repostCount
    , Data.Aeson.Key.fromString "threadgate" Data.Aeson..?= threadgate
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data ReasonPin = ReasonPin
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReasonPin where
  parseJSON = Data.Aeson.withObject "ReasonPin" $ \v -> do
    pure $ ReasonPin

instance Data.Aeson.ToJSON ReasonPin where
  toJSON (ReasonPin) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (ReasonPin) =
    Data.Aeson.pairs $
      mconcat
        []

reasonPin'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReasonPin -> kv
reasonPin'AesonFields (ReasonPin) =
  mconcat
    []

data ReasonRepost = ReasonRepost
  { by :: App.Bsky.Actor.Defs.ProfileViewBasic
  , indexedAt :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReasonRepost where
  parseJSON = Data.Aeson.withObject "ReasonRepost" $ \v -> do
    by <- v Data.Aeson..: Data.Aeson.Key.fromString "by"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    pure $ ReasonRepost by indexedAt

instance Data.Aeson.ToJSON ReasonRepost where
  toJSON (ReasonRepost by indexedAt) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "by" Data.Aeson..= by
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        ]
  toEncoding (ReasonRepost by indexedAt) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "by" Data.Aeson..= by
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        ]

reasonRepost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReasonRepost -> kv
reasonRepost'AesonFields (ReasonRepost by indexedAt) =
  mconcat
    [ Data.Aeson.Key.fromString "by" Data.Aeson..= by
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    ]

data ReplyRef = ReplyRef
  { grandparentAuthor :: Maybe App.Bsky.Actor.Defs.ProfileViewBasic
  , parent :: ReplyRefParentKind
  , root :: ReplyRefRootKind
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReplyRef where
  parseJSON = Data.Aeson.withObject "ReplyRef" $ \v -> do
    grandparentAuthor <- v Data.Aeson..:? Data.Aeson.Key.fromString "grandparentAuthor"
    parent <- v Data.Aeson..: Data.Aeson.Key.fromString "parent"
    root <- v Data.Aeson..: Data.Aeson.Key.fromString "root"
    pure $ ReplyRef grandparentAuthor parent root

instance Data.Aeson.ToJSON ReplyRef where
  toJSON (ReplyRef grandparentAuthor parent root) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "grandparentAuthor" Data.Aeson..?= grandparentAuthor
        , Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
        , Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]
  toEncoding (ReplyRef grandparentAuthor parent root) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "grandparentAuthor" Data.Aeson..?= grandparentAuthor
        , Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
        , Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]

replyRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReplyRef -> kv
replyRef'AesonFields (ReplyRef grandparentAuthor parent root) =
  mconcat
    [ Data.Aeson.Key.fromString "grandparentAuthor" Data.Aeson..?= grandparentAuthor
    , Data.Aeson.Key.fromString "parent" Data.Aeson..= parent
    , Data.Aeson.Key.fromString "root" Data.Aeson..= root
    ]

data SkeletonFeedPost = SkeletonFeedPost
  { feedContext :: Maybe Data.Text.Text
  , post :: Data.Text.Text
  , reason :: Maybe SkeletonFeedPostReasonKind
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonFeedPost where
  parseJSON = Data.Aeson.withObject "SkeletonFeedPost" $ \v -> do
    feedContext <- v Data.Aeson..:? Data.Aeson.Key.fromString "feedContext"
    post <- v Data.Aeson..: Data.Aeson.Key.fromString "post"
    reason <- v Data.Aeson..:? Data.Aeson.Key.fromString "reason"
    pure $ SkeletonFeedPost feedContext post reason

instance Data.Aeson.ToJSON SkeletonFeedPost where
  toJSON (SkeletonFeedPost feedContext post reason) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
        ]
  toEncoding (SkeletonFeedPost feedContext post reason) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
        ]

skeletonFeedPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonFeedPost -> kv
skeletonFeedPost'AesonFields (SkeletonFeedPost feedContext post reason) =
  mconcat
    [ Data.Aeson.Key.fromString "feedContext" Data.Aeson..?= feedContext
    , Data.Aeson.Key.fromString "post" Data.Aeson..= post
    , Data.Aeson.Key.fromString "reason" Data.Aeson..?= reason
    ]

data SkeletonReasonPin = SkeletonReasonPin
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonReasonPin where
  parseJSON = Data.Aeson.withObject "SkeletonReasonPin" $ \v -> do
    pure $ SkeletonReasonPin

instance Data.Aeson.ToJSON SkeletonReasonPin where
  toJSON (SkeletonReasonPin) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (SkeletonReasonPin) =
    Data.Aeson.pairs $
      mconcat
        []

skeletonReasonPin'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonReasonPin -> kv
skeletonReasonPin'AesonFields (SkeletonReasonPin) =
  mconcat
    []

data SkeletonReasonRepost = SkeletonReasonRepost
  { repost :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonReasonRepost where
  parseJSON = Data.Aeson.withObject "SkeletonReasonRepost" $ \v -> do
    repost <- v Data.Aeson..: Data.Aeson.Key.fromString "repost"
    pure $ SkeletonReasonRepost repost

instance Data.Aeson.ToJSON SkeletonReasonRepost where
  toJSON (SkeletonReasonRepost repost) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "repost" Data.Aeson..= repost
        ]
  toEncoding (SkeletonReasonRepost repost) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "repost" Data.Aeson..= repost
        ]

skeletonReasonRepost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonReasonRepost -> kv
skeletonReasonRepost'AesonFields (SkeletonReasonRepost repost) =
  mconcat
    [ Data.Aeson.Key.fromString "repost" Data.Aeson..= repost
    ]

data ThreadViewPost = ThreadViewPost
  { parent :: Maybe ThreadViewPostParentKind
  , post :: PostView
  , replies :: Maybe [ThreadViewPostRepliesKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadViewPost where
  parseJSON = Data.Aeson.withObject "ThreadViewPost" $ \v -> do
    parent <- v Data.Aeson..:? Data.Aeson.Key.fromString "parent"
    post <- v Data.Aeson..: Data.Aeson.Key.fromString "post"
    replies <- v Data.Aeson..:? Data.Aeson.Key.fromString "replies"
    pure $ ThreadViewPost parent post replies

instance Data.Aeson.ToJSON ThreadViewPost where
  toJSON (ThreadViewPost parent post replies) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "parent" Data.Aeson..?= parent
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "replies" Data.Aeson..?= replies
        ]
  toEncoding (ThreadViewPost parent post replies) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "parent" Data.Aeson..?= parent
        , Data.Aeson.Key.fromString "post" Data.Aeson..= post
        , Data.Aeson.Key.fromString "replies" Data.Aeson..?= replies
        ]

threadViewPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadViewPost -> kv
threadViewPost'AesonFields (ThreadViewPost parent post replies) =
  mconcat
    [ Data.Aeson.Key.fromString "parent" Data.Aeson..?= parent
    , Data.Aeson.Key.fromString "post" Data.Aeson..= post
    , Data.Aeson.Key.fromString "replies" Data.Aeson..?= replies
    ]

data ThreadgateView = ThreadgateView
  { cid :: Maybe Data.Text.Text
  , lists :: Maybe [App.Bsky.Graph.Defs.ListViewBasic]
  , record :: Maybe Data.Aeson.Value
  , uri :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadgateView where
  parseJSON = Data.Aeson.withObject "ThreadgateView" $ \v -> do
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    lists <- v Data.Aeson..:? Data.Aeson.Key.fromString "lists"
    record <- v Data.Aeson..:? Data.Aeson.Key.fromString "record"
    uri <- v Data.Aeson..:? Data.Aeson.Key.fromString "uri"
    pure $ ThreadgateView cid lists record uri

instance Data.Aeson.ToJSON ThreadgateView where
  toJSON (ThreadgateView cid lists record uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
        , Data.Aeson.Key.fromString "record" Data.Aeson..?= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..?= uri
        ]
  toEncoding (ThreadgateView cid lists record uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
        , Data.Aeson.Key.fromString "record" Data.Aeson..?= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..?= uri
        ]

threadgateView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ThreadgateView -> kv
threadgateView'AesonFields (ThreadgateView cid lists record uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "lists" Data.Aeson..?= lists
    , Data.Aeson.Key.fromString "record" Data.Aeson..?= record
    , Data.Aeson.Key.fromString "uri" Data.Aeson..?= uri
    ]

data ViewerState = ViewerState
  { embeddingDisabled :: Maybe Bool
  , like :: Maybe Data.Text.Text
  , pinned :: Maybe Bool
  , replyDisabled :: Maybe Bool
  , repost :: Maybe Data.Text.Text
  , threadMuted :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ViewerState where
  parseJSON = Data.Aeson.withObject "ViewerState" $ \v -> do
    embeddingDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "embeddingDisabled"
    like <- v Data.Aeson..:? Data.Aeson.Key.fromString "like"
    pinned <- v Data.Aeson..:? Data.Aeson.Key.fromString "pinned"
    replyDisabled <- v Data.Aeson..:? Data.Aeson.Key.fromString "replyDisabled"
    repost <- v Data.Aeson..:? Data.Aeson.Key.fromString "repost"
    threadMuted <- v Data.Aeson..:? Data.Aeson.Key.fromString "threadMuted"
    pure $ ViewerState embeddingDisabled like pinned replyDisabled repost threadMuted

instance Data.Aeson.ToJSON ViewerState where
  toJSON (ViewerState embeddingDisabled like pinned replyDisabled repost threadMuted) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "embeddingDisabled" Data.Aeson..?= embeddingDisabled
        , Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        , Data.Aeson.Key.fromString "pinned" Data.Aeson..?= pinned
        , Data.Aeson.Key.fromString "replyDisabled" Data.Aeson..?= replyDisabled
        , Data.Aeson.Key.fromString "repost" Data.Aeson..?= repost
        , Data.Aeson.Key.fromString "threadMuted" Data.Aeson..?= threadMuted
        ]
  toEncoding (ViewerState embeddingDisabled like pinned replyDisabled repost threadMuted) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "embeddingDisabled" Data.Aeson..?= embeddingDisabled
        , Data.Aeson.Key.fromString "like" Data.Aeson..?= like
        , Data.Aeson.Key.fromString "pinned" Data.Aeson..?= pinned
        , Data.Aeson.Key.fromString "replyDisabled" Data.Aeson..?= replyDisabled
        , Data.Aeson.Key.fromString "repost" Data.Aeson..?= repost
        , Data.Aeson.Key.fromString "threadMuted" Data.Aeson..?= threadMuted
        ]

viewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ViewerState -> kv
viewerState'AesonFields (ViewerState embeddingDisabled like pinned replyDisabled repost threadMuted) =
  mconcat
    [ Data.Aeson.Key.fromString "embeddingDisabled" Data.Aeson..?= embeddingDisabled
    , Data.Aeson.Key.fromString "like" Data.Aeson..?= like
    , Data.Aeson.Key.fromString "pinned" Data.Aeson..?= pinned
    , Data.Aeson.Key.fromString "replyDisabled" Data.Aeson..?= replyDisabled
    , Data.Aeson.Key.fromString "repost" Data.Aeson..?= repost
    , Data.Aeson.Key.fromString "threadMuted" Data.Aeson..?= threadMuted
    ]

data FeedViewPostReasonKind
  = FeedViewPostReasonKindReasonRepost ReasonRepost
  | FeedViewPostReasonKindReasonPin ReasonPin
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON FeedViewPostReasonKind where
  parseJSON = Data.Aeson.withObject "FeedViewPostReasonKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#reasonRepost" -> FeedViewPostReasonKindReasonRepost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#reasonPin" -> FeedViewPostReasonKindReasonPin <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON FeedViewPostReasonKind where
  toJSON = \case
    FeedViewPostReasonKindReasonRepost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#reasonRepost") <> (App.Bsky.Feed.Defs.reasonRepost'AesonFields v))
    FeedViewPostReasonKindReasonPin v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#reasonPin") <> (App.Bsky.Feed.Defs.reasonPin'AesonFields v))
  toEncoding = \case
    FeedViewPostReasonKindReasonRepost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#reasonRepost") <> (App.Bsky.Feed.Defs.reasonRepost'AesonFields v))
    FeedViewPostReasonKindReasonPin v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#reasonPin") <> (App.Bsky.Feed.Defs.reasonPin'AesonFields v))

data PostViewEmbedKind
  = PostViewEmbedKindImagesView App.Bsky.Embed.Images.View
  | PostViewEmbedKindVideoView App.Bsky.Embed.Video.View
  | PostViewEmbedKindExternalView App.Bsky.Embed.External.View
  | PostViewEmbedKindRecordView App.Bsky.Embed.Record.View
  | PostViewEmbedKindRecordWithMediaView App.Bsky.Embed.RecordWithMedia.View
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON PostViewEmbedKind where
  parseJSON = Data.Aeson.withObject "PostViewEmbedKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.embed.images#view" -> PostViewEmbedKindImagesView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.video#view" -> PostViewEmbedKindVideoView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.external#view" -> PostViewEmbedKindExternalView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.record#view" -> PostViewEmbedKindRecordView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.embed.recordWithMedia#view" -> PostViewEmbedKindRecordWithMediaView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON PostViewEmbedKind where
  toJSON = \case
    PostViewEmbedKindImagesView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    PostViewEmbedKindVideoView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    PostViewEmbedKindExternalView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
    PostViewEmbedKindRecordView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
    PostViewEmbedKindRecordWithMediaView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#view") <> (App.Bsky.Embed.RecordWithMedia.view'AesonFields v))
  toEncoding = \case
    PostViewEmbedKindImagesView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.images#view") <> (App.Bsky.Embed.Images.view'AesonFields v))
    PostViewEmbedKindVideoView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.video#view") <> (App.Bsky.Embed.Video.view'AesonFields v))
    PostViewEmbedKindExternalView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.external#view") <> (App.Bsky.Embed.External.view'AesonFields v))
    PostViewEmbedKindRecordView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.record#view") <> (App.Bsky.Embed.Record.view'AesonFields v))
    PostViewEmbedKindRecordWithMediaView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.embed.recordWithMedia#view") <> (App.Bsky.Embed.RecordWithMedia.view'AesonFields v))

data ReplyRefParentKind
  = ReplyRefParentKindPostView PostView
  | ReplyRefParentKindNotFoundPost NotFoundPost
  | ReplyRefParentKindBlockedPost BlockedPost
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReplyRefParentKind where
  parseJSON = Data.Aeson.withObject "ReplyRefParentKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#postView" -> ReplyRefParentKindPostView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#notFoundPost" -> ReplyRefParentKindNotFoundPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#blockedPost" -> ReplyRefParentKindBlockedPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ReplyRefParentKind where
  toJSON = \case
    ReplyRefParentKindPostView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#postView") <> (App.Bsky.Feed.Defs.postView'AesonFields v))
    ReplyRefParentKindNotFoundPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ReplyRefParentKindBlockedPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
  toEncoding = \case
    ReplyRefParentKindPostView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#postView") <> (App.Bsky.Feed.Defs.postView'AesonFields v))
    ReplyRefParentKindNotFoundPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ReplyRefParentKindBlockedPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))

data ReplyRefRootKind
  = ReplyRefRootKindPostView PostView
  | ReplyRefRootKindNotFoundPost NotFoundPost
  | ReplyRefRootKindBlockedPost BlockedPost
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ReplyRefRootKind where
  parseJSON = Data.Aeson.withObject "ReplyRefRootKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#postView" -> ReplyRefRootKindPostView <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#notFoundPost" -> ReplyRefRootKindNotFoundPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#blockedPost" -> ReplyRefRootKindBlockedPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ReplyRefRootKind where
  toJSON = \case
    ReplyRefRootKindPostView v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#postView") <> (App.Bsky.Feed.Defs.postView'AesonFields v))
    ReplyRefRootKindNotFoundPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ReplyRefRootKindBlockedPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
  toEncoding = \case
    ReplyRefRootKindPostView v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#postView") <> (App.Bsky.Feed.Defs.postView'AesonFields v))
    ReplyRefRootKindNotFoundPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ReplyRefRootKindBlockedPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))

data SkeletonFeedPostReasonKind
  = SkeletonFeedPostReasonKindSkeletonReasonRepost SkeletonReasonRepost
  | SkeletonFeedPostReasonKindSkeletonReasonPin SkeletonReasonPin
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonFeedPostReasonKind where
  parseJSON = Data.Aeson.withObject "SkeletonFeedPostReasonKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#skeletonReasonRepost" -> SkeletonFeedPostReasonKindSkeletonReasonRepost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#skeletonReasonPin" -> SkeletonFeedPostReasonKindSkeletonReasonPin <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON SkeletonFeedPostReasonKind where
  toJSON = \case
    SkeletonFeedPostReasonKindSkeletonReasonRepost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#skeletonReasonRepost") <> (App.Bsky.Feed.Defs.skeletonReasonRepost'AesonFields v))
    SkeletonFeedPostReasonKindSkeletonReasonPin v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#skeletonReasonPin") <> (App.Bsky.Feed.Defs.skeletonReasonPin'AesonFields v))
  toEncoding = \case
    SkeletonFeedPostReasonKindSkeletonReasonRepost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#skeletonReasonRepost") <> (App.Bsky.Feed.Defs.skeletonReasonRepost'AesonFields v))
    SkeletonFeedPostReasonKindSkeletonReasonPin v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#skeletonReasonPin") <> (App.Bsky.Feed.Defs.skeletonReasonPin'AesonFields v))

data ThreadViewPostParentKind
  = ThreadViewPostParentKindThreadViewPost ThreadViewPost
  | ThreadViewPostParentKindNotFoundPost NotFoundPost
  | ThreadViewPostParentKindBlockedPost BlockedPost
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadViewPostParentKind where
  parseJSON = Data.Aeson.withObject "ThreadViewPostParentKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#threadViewPost" -> ThreadViewPostParentKindThreadViewPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#notFoundPost" -> ThreadViewPostParentKindNotFoundPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#blockedPost" -> ThreadViewPostParentKindBlockedPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ThreadViewPostParentKind where
  toJSON = \case
    ThreadViewPostParentKindThreadViewPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    ThreadViewPostParentKindNotFoundPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ThreadViewPostParentKindBlockedPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
  toEncoding = \case
    ThreadViewPostParentKindThreadViewPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    ThreadViewPostParentKindNotFoundPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ThreadViewPostParentKindBlockedPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))

data ThreadViewPostRepliesKind
  = ThreadViewPostRepliesKindThreadViewPost ThreadViewPost
  | ThreadViewPostRepliesKindNotFoundPost NotFoundPost
  | ThreadViewPostRepliesKindBlockedPost BlockedPost
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ThreadViewPostRepliesKind where
  parseJSON = Data.Aeson.withObject "ThreadViewPostRepliesKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "app.bsky.feed.defs#threadViewPost" -> ThreadViewPostRepliesKindThreadViewPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#notFoundPost" -> ThreadViewPostRepliesKindNotFoundPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "app.bsky.feed.defs#blockedPost" -> ThreadViewPostRepliesKindBlockedPost <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON ThreadViewPostRepliesKind where
  toJSON = \case
    ThreadViewPostRepliesKindThreadViewPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    ThreadViewPostRepliesKindNotFoundPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ThreadViewPostRepliesKindBlockedPost v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
  toEncoding = \case
    ThreadViewPostRepliesKindThreadViewPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#threadViewPost") <> (App.Bsky.Feed.Defs.threadViewPost'AesonFields v))
    ThreadViewPostRepliesKindNotFoundPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#notFoundPost") <> (App.Bsky.Feed.Defs.notFoundPost'AesonFields v))
    ThreadViewPostRepliesKindBlockedPost v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "app.bsky.feed.defs#blockedPost") <> (App.Bsky.Feed.Defs.blockedPost'AesonFields v))
