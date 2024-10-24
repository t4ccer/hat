{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Graph.Defs where

import {-# SOURCE #-} qualified App.Bsky.Actor.Defs
import {-# SOURCE #-} qualified App.Bsky.Feed.Defs
import {-# SOURCE #-} qualified App.Bsky.Richtext.Facet
import {-# SOURCE #-} qualified Com.Atproto.Label.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListItemView = ListItemView
  { subject :: App.Bsky.Actor.Defs.ProfileView
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListItemView where
  parseJSON = Data.Aeson.withObject "ListItemView" $ \v -> do
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ ListItemView subject uri

instance Data.Aeson.ToJSON ListItemView where
  toJSON (ListItemView subject uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (ListItemView subject uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

listItemView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListItemView -> kv
listItemView'AesonFields (ListItemView subject uri) =
  mconcat
    [ Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

newtype ListPurpose = ListPurpose
  { getListPurpose :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListPurpose where
  parseJSON = Data.Aeson.withText "ListPurpose" $ pure . ListPurpose

instance Data.Aeson.ToJSON ListPurpose where
  toJSON (ListPurpose getListPurpose) = Data.Aeson.toJSON getListPurpose
  toEncoding (ListPurpose getListPurpose) = Data.Aeson.toEncoding getListPurpose

data ListView = ListView
  { avatar :: Maybe Data.Text.Text
  , cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileView
  , description :: Maybe Data.Text.Text
  , descriptionFacets :: Maybe [App.Bsky.Richtext.Facet.Facet]
  , indexedAt :: Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , listItemCount :: Maybe Integer
  , name :: Data.Text.Text
  , purpose :: ListPurpose
  , uri :: Data.Text.Text
  , viewer :: Maybe ListViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListView where
  parseJSON = Data.Aeson.withObject "ListView" $ \v -> do
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    description <- v Data.Aeson..:? Data.Aeson.Key.fromString "description"
    descriptionFacets <- v Data.Aeson..:? Data.Aeson.Key.fromString "descriptionFacets"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    listItemCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "listItemCount"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    purpose <- v Data.Aeson..: Data.Aeson.Key.fromString "purpose"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ListView avatar cid creator description descriptionFacets indexedAt labels listItemCount name purpose uri viewer

instance Data.Aeson.ToJSON ListView where
  toJSON (ListView avatar cid creator description descriptionFacets indexedAt labels listItemCount name purpose uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ListView avatar cid creator description descriptionFacets indexedAt labels listItemCount name purpose uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
        , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

listView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListView -> kv
listView'AesonFields (ListView avatar cid creator description descriptionFacets indexedAt labels listItemCount name purpose uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "description" Data.Aeson..?= description
    , Data.Aeson.Key.fromString "descriptionFacets" Data.Aeson..?= descriptionFacets
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data ListViewBasic = ListViewBasic
  { avatar :: Maybe Data.Text.Text
  , cid :: Data.Text.Text
  , indexedAt :: Maybe Data.Text.Text
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , listItemCount :: Maybe Integer
  , name :: Data.Text.Text
  , purpose :: ListPurpose
  , uri :: Data.Text.Text
  , viewer :: Maybe ListViewerState
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListViewBasic where
  parseJSON = Data.Aeson.withObject "ListViewBasic" $ \v -> do
    avatar <- v Data.Aeson..:? Data.Aeson.Key.fromString "avatar"
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    indexedAt <- v Data.Aeson..:? Data.Aeson.Key.fromString "indexedAt"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    listItemCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "listItemCount"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    purpose <- v Data.Aeson..: Data.Aeson.Key.fromString "purpose"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    viewer <- v Data.Aeson..:? Data.Aeson.Key.fromString "viewer"
    pure $ ListViewBasic avatar cid indexedAt labels listItemCount name purpose uri viewer

instance Data.Aeson.ToJSON ListViewBasic where
  toJSON (ListViewBasic avatar cid indexedAt labels listItemCount name purpose uri viewer) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]
  toEncoding (ListViewBasic avatar cid indexedAt labels listItemCount name purpose uri viewer) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
        , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
        ]

listViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListViewBasic -> kv
listViewBasic'AesonFields (ListViewBasic avatar cid indexedAt labels listItemCount name purpose uri viewer) =
  mconcat
    [ Data.Aeson.Key.fromString "avatar" Data.Aeson..?= avatar
    , Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..?= indexedAt
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    , Data.Aeson.Key.fromString "purpose" Data.Aeson..= purpose
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "viewer" Data.Aeson..?= viewer
    ]

data ListViewerState = ListViewerState
  { blocked :: Maybe Data.Text.Text
  , muted :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListViewerState where
  parseJSON = Data.Aeson.withObject "ListViewerState" $ \v -> do
    blocked <- v Data.Aeson..:? Data.Aeson.Key.fromString "blocked"
    muted <- v Data.Aeson..:? Data.Aeson.Key.fromString "muted"
    pure $ ListViewerState blocked muted

instance Data.Aeson.ToJSON ListViewerState where
  toJSON (ListViewerState blocked muted) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blocked" Data.Aeson..?= blocked
        , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
        ]
  toEncoding (ListViewerState blocked muted) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blocked" Data.Aeson..?= blocked
        , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
        ]

listViewerState'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListViewerState -> kv
listViewerState'AesonFields (ListViewerState blocked muted) =
  mconcat
    [ Data.Aeson.Key.fromString "blocked" Data.Aeson..?= blocked
    , Data.Aeson.Key.fromString "muted" Data.Aeson..?= muted
    ]

data NotFoundActor = NotFoundActor
  { actor :: Data.Text.Text
  , notFound :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON NotFoundActor where
  parseJSON = Data.Aeson.withObject "NotFoundActor" $ \v -> do
    actor <- v Data.Aeson..: Data.Aeson.Key.fromString "actor"
    notFound <- v Data.Aeson..: Data.Aeson.Key.fromString "notFound"
    pure $ NotFoundActor actor notFound

instance Data.Aeson.ToJSON NotFoundActor where
  toJSON (NotFoundActor actor notFound) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
        , Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        ]
  toEncoding (NotFoundActor actor notFound) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
        , Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
        ]

notFoundActor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => NotFoundActor -> kv
notFoundActor'AesonFields (NotFoundActor actor notFound) =
  mconcat
    [ Data.Aeson.Key.fromString "actor" Data.Aeson..= actor
    , Data.Aeson.Key.fromString "notFound" Data.Aeson..= notFound
    ]

data Relationship = Relationship
  { did :: Data.Text.Text
  , followedBy :: Maybe Data.Text.Text
  , following :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Relationship where
  parseJSON = Data.Aeson.withObject "Relationship" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    followedBy <- v Data.Aeson..:? Data.Aeson.Key.fromString "followedBy"
    following <- v Data.Aeson..:? Data.Aeson.Key.fromString "following"
    pure $ Relationship did followedBy following

instance Data.Aeson.ToJSON Relationship where
  toJSON (Relationship did followedBy following) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
        , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
        ]
  toEncoding (Relationship did followedBy following) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
        , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
        ]

relationship'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Relationship -> kv
relationship'AesonFields (Relationship did followedBy following) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "followedBy" Data.Aeson..?= followedBy
    , Data.Aeson.Key.fromString "following" Data.Aeson..?= following
    ]

data StarterPackView = StarterPackView
  { cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileViewBasic
  , feeds :: Maybe [App.Bsky.Feed.Defs.GeneratorView]
  , indexedAt :: Data.Text.Text
  , joinedAllTimeCount :: Maybe Integer
  , joinedWeekCount :: Maybe Integer
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , list :: Maybe ListViewBasic
  , listItemsSample :: Maybe [ListItemView]
  , record :: Data.Aeson.Value
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON StarterPackView where
  parseJSON = Data.Aeson.withObject "StarterPackView" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    feeds <- v Data.Aeson..:? Data.Aeson.Key.fromString "feeds"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    joinedAllTimeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedAllTimeCount"
    joinedWeekCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedWeekCount"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    list <- v Data.Aeson..:? Data.Aeson.Key.fromString "list"
    listItemsSample <- v Data.Aeson..:? Data.Aeson.Key.fromString "listItemsSample"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ StarterPackView cid creator feeds indexedAt joinedAllTimeCount joinedWeekCount labels list listItemsSample record uri

instance Data.Aeson.ToJSON StarterPackView where
  toJSON (StarterPackView cid creator feeds indexedAt joinedAllTimeCount joinedWeekCount labels list listItemsSample record uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
        , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "list" Data.Aeson..?= list
        , Data.Aeson.Key.fromString "listItemsSample" Data.Aeson..?= listItemsSample
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (StarterPackView cid creator feeds indexedAt joinedAllTimeCount joinedWeekCount labels list listItemsSample record uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
        , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "list" Data.Aeson..?= list
        , Data.Aeson.Key.fromString "listItemsSample" Data.Aeson..?= listItemsSample
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

starterPackView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StarterPackView -> kv
starterPackView'AesonFields (StarterPackView cid creator feeds indexedAt joinedAllTimeCount joinedWeekCount labels list listItemsSample record uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "feeds" Data.Aeson..?= feeds
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
    , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "list" Data.Aeson..?= list
    , Data.Aeson.Key.fromString "listItemsSample" Data.Aeson..?= listItemsSample
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]

data StarterPackViewBasic = StarterPackViewBasic
  { cid :: Data.Text.Text
  , creator :: App.Bsky.Actor.Defs.ProfileViewBasic
  , indexedAt :: Data.Text.Text
  , joinedAllTimeCount :: Maybe Integer
  , joinedWeekCount :: Maybe Integer
  , labels :: Maybe [Com.Atproto.Label.Defs.Label]
  , listItemCount :: Maybe Integer
  , record :: Data.Aeson.Value
  , uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON StarterPackViewBasic where
  parseJSON = Data.Aeson.withObject "StarterPackViewBasic" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    creator <- v Data.Aeson..: Data.Aeson.Key.fromString "creator"
    indexedAt <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedAt"
    joinedAllTimeCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedAllTimeCount"
    joinedWeekCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "joinedWeekCount"
    labels <- v Data.Aeson..:? Data.Aeson.Key.fromString "labels"
    listItemCount <- v Data.Aeson..:? Data.Aeson.Key.fromString "listItemCount"
    record <- v Data.Aeson..: Data.Aeson.Key.fromString "record"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ StarterPackViewBasic cid creator indexedAt joinedAllTimeCount joinedWeekCount labels listItemCount record uri

instance Data.Aeson.ToJSON StarterPackViewBasic where
  toJSON (StarterPackViewBasic cid creator indexedAt joinedAllTimeCount joinedWeekCount labels listItemCount record uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
        , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (StarterPackViewBasic cid creator indexedAt joinedAllTimeCount joinedWeekCount labels listItemCount record uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
        , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
        , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
        , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
        , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
        , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
        , Data.Aeson.Key.fromString "record" Data.Aeson..= record
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

starterPackViewBasic'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => StarterPackViewBasic -> kv
starterPackViewBasic'AesonFields (StarterPackViewBasic cid creator indexedAt joinedAllTimeCount joinedWeekCount labels listItemCount record uri) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "creator" Data.Aeson..= creator
    , Data.Aeson.Key.fromString "indexedAt" Data.Aeson..= indexedAt
    , Data.Aeson.Key.fromString "joinedAllTimeCount" Data.Aeson..?= joinedAllTimeCount
    , Data.Aeson.Key.fromString "joinedWeekCount" Data.Aeson..?= joinedWeekCount
    , Data.Aeson.Key.fromString "labels" Data.Aeson..?= labels
    , Data.Aeson.Key.fromString "listItemCount" Data.Aeson..?= listItemCount
    , Data.Aeson.Key.fromString "record" Data.Aeson..= record
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]
