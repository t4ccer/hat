{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.SubscribeRepos where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Account = Account
  { active :: Bool
  , did :: Data.Text.Text
  , seq :: Integer
  , status :: Maybe Data.Text.Text
  , time :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Account where
  parseJSON = Data.Aeson.withObject "Account" $ \v -> do
    active <- v Data.Aeson..: Data.Aeson.Key.fromString "active"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    status <- v Data.Aeson..:? Data.Aeson.Key.fromString "status"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    pure $ Account active did seq status time

instance Data.Aeson.ToJSON Account where
  toJSON (Account active did seq status time) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]
  toEncoding (Account active did seq status time) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]

account'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Account -> kv
account'AesonFields (Account active did seq status time) =
  mconcat
    [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    ]

data Commit = Commit
  { blobs :: [Data.Text.Text]
  , blocks :: Data.ByteString.ByteString
  , commit :: Data.Text.Text
  , ops :: [RepoOp]
  , prev :: Maybe Data.Text.Text
  , rebase :: Bool
  , repo :: Data.Text.Text
  , rev :: Data.Text.Text
  , seq :: Integer
  , since :: Maybe Data.Text.Text
  , time :: Data.Text.Text
  , tooBig :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Commit where
  parseJSON = Data.Aeson.withObject "Commit" $ \v -> do
    blobs <- v Data.Aeson..: Data.Aeson.Key.fromString "blobs"
    blocks <- Data.Text.Encoding.encodeUtf8 <$> v Data.Aeson..: Data.Aeson.Key.fromString "blocks"
    commit <- v Data.Aeson..: Data.Aeson.Key.fromString "commit"
    ops <- v Data.Aeson..: Data.Aeson.Key.fromString "ops"
    prev <- v Data.Aeson..:? Data.Aeson.Key.fromString "prev"
    rebase <- v Data.Aeson..: Data.Aeson.Key.fromString "rebase"
    repo <- v Data.Aeson..: Data.Aeson.Key.fromString "repo"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    since <- v Data.Aeson..:? Data.Aeson.Key.fromString "since"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    tooBig <- v Data.Aeson..: Data.Aeson.Key.fromString "tooBig"
    pure $ Commit blobs blocks commit ops prev rebase repo rev seq since time tooBig

instance Data.Aeson.ToJSON Commit where
  toJSON (Commit blobs blocks commit ops prev rebase repo rev seq since time tooBig) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "blocks" Data.Aeson..= Data.Text.Encoding.decodeUtf8 blocks
        , Data.Aeson.Key.fromString "commit" Data.Aeson..= commit
        , Data.Aeson.Key.fromString "ops" Data.Aeson..= ops
        , Data.Aeson.Key.fromString "prev" Data.Aeson..?= prev
        , Data.Aeson.Key.fromString "rebase" Data.Aeson..= rebase
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "since" Data.Aeson..?= since
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        , Data.Aeson.Key.fromString "tooBig" Data.Aeson..= tooBig
        ]
  toEncoding (Commit blobs blocks commit ops prev rebase repo rev seq since time tooBig) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
        , Data.Aeson.Key.fromString "blocks" Data.Aeson..= Data.Text.Encoding.decodeUtf8 blocks
        , Data.Aeson.Key.fromString "commit" Data.Aeson..= commit
        , Data.Aeson.Key.fromString "ops" Data.Aeson..= ops
        , Data.Aeson.Key.fromString "prev" Data.Aeson..?= prev
        , Data.Aeson.Key.fromString "rebase" Data.Aeson..= rebase
        , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "since" Data.Aeson..?= since
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        , Data.Aeson.Key.fromString "tooBig" Data.Aeson..= tooBig
        ]

commit'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Commit -> kv
commit'AesonFields (Commit blobs blocks commit ops prev rebase repo rev seq since time tooBig) =
  mconcat
    [ Data.Aeson.Key.fromString "blobs" Data.Aeson..= blobs
    , Data.Aeson.Key.fromString "blocks" Data.Aeson..= Data.Text.Encoding.decodeUtf8 blocks
    , Data.Aeson.Key.fromString "commit" Data.Aeson..= commit
    , Data.Aeson.Key.fromString "ops" Data.Aeson..= ops
    , Data.Aeson.Key.fromString "prev" Data.Aeson..?= prev
    , Data.Aeson.Key.fromString "rebase" Data.Aeson..= rebase
    , Data.Aeson.Key.fromString "repo" Data.Aeson..= repo
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "since" Data.Aeson..?= since
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    , Data.Aeson.Key.fromString "tooBig" Data.Aeson..= tooBig
    ]

data Handle = Handle
  { did :: Data.Text.Text
  , handle :: Data.Text.Text
  , seq :: Integer
  , time :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Handle where
  parseJSON = Data.Aeson.withObject "Handle" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    pure $ Handle did handle seq time

instance Data.Aeson.ToJSON Handle where
  toJSON (Handle did handle seq time) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]
  toEncoding (Handle did handle seq time) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]

handle'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Handle -> kv
handle'AesonFields (Handle did handle seq time) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    ]

data Identity = Identity
  { did :: Data.Text.Text
  , handle :: Maybe Data.Text.Text
  , seq :: Integer
  , time :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Identity where
  parseJSON = Data.Aeson.withObject "Identity" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    handle <- v Data.Aeson..:? Data.Aeson.Key.fromString "handle"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    pure $ Identity did handle seq time

instance Data.Aeson.ToJSON Identity where
  toJSON (Identity did handle seq time) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "handle" Data.Aeson..?= handle
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]
  toEncoding (Identity did handle seq time) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "handle" Data.Aeson..?= handle
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]

identity'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Identity -> kv
identity'AesonFields (Identity did handle seq time) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "handle" Data.Aeson..?= handle
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    ]

data Info = Info
  { message :: Maybe Data.Text.Text
  , name :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Info where
  parseJSON = Data.Aeson.withObject "Info" $ \v -> do
    message <- v Data.Aeson..:? Data.Aeson.Key.fromString "message"
    name <- v Data.Aeson..: Data.Aeson.Key.fromString "name"
    pure $ Info message name

instance Data.Aeson.ToJSON Info where
  toJSON (Info message name) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]
  toEncoding (Info message name) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
        , Data.Aeson.Key.fromString "name" Data.Aeson..= name
        ]

info'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Info -> kv
info'AesonFields (Info message name) =
  mconcat
    [ Data.Aeson.Key.fromString "message" Data.Aeson..?= message
    , Data.Aeson.Key.fromString "name" Data.Aeson..= name
    ]

data Migrate = Migrate
  { did :: Data.Text.Text
  , migrateTo :: Maybe Data.Text.Text
  , seq :: Integer
  , time :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Migrate where
  parseJSON = Data.Aeson.withObject "Migrate" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    migrateTo <- v Data.Aeson..:? Data.Aeson.Key.fromString "migrateTo"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    pure $ Migrate did migrateTo seq time

instance Data.Aeson.ToJSON Migrate where
  toJSON (Migrate did migrateTo seq time) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "migrateTo" Data.Aeson..?= migrateTo
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]
  toEncoding (Migrate did migrateTo seq time) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "migrateTo" Data.Aeson..?= migrateTo
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]

migrate'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Migrate -> kv
migrate'AesonFields (Migrate did migrateTo seq time) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "migrateTo" Data.Aeson..?= migrateTo
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    ]

data RepoOp = RepoOp
  { action :: Data.Text.Text
  , cid :: Maybe Data.Text.Text
  , path :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON RepoOp where
  parseJSON = Data.Aeson.withObject "RepoOp" $ \v -> do
    action <- v Data.Aeson..: Data.Aeson.Key.fromString "action"
    cid <- v Data.Aeson..:? Data.Aeson.Key.fromString "cid"
    path <- v Data.Aeson..: Data.Aeson.Key.fromString "path"
    pure $ RepoOp action cid path

instance Data.Aeson.ToJSON RepoOp where
  toJSON (RepoOp action cid path) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "action" Data.Aeson..= action
        , Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "path" Data.Aeson..= path
        ]
  toEncoding (RepoOp action cid path) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "action" Data.Aeson..= action
        , Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
        , Data.Aeson.Key.fromString "path" Data.Aeson..= path
        ]

repoOp'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => RepoOp -> kv
repoOp'AesonFields (RepoOp action cid path) =
  mconcat
    [ Data.Aeson.Key.fromString "action" Data.Aeson..= action
    , Data.Aeson.Key.fromString "cid" Data.Aeson..?= cid
    , Data.Aeson.Key.fromString "path" Data.Aeson..= path
    ]

data Tombstone = Tombstone
  { did :: Data.Text.Text
  , seq :: Integer
  , time :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Tombstone where
  parseJSON = Data.Aeson.withObject "Tombstone" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    seq <- v Data.Aeson..: Data.Aeson.Key.fromString "seq"
    time <- v Data.Aeson..: Data.Aeson.Key.fromString "time"
    pure $ Tombstone did seq time

instance Data.Aeson.ToJSON Tombstone where
  toJSON (Tombstone did seq time) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]
  toEncoding (Tombstone did seq time) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
        , Data.Aeson.Key.fromString "time" Data.Aeson..= time
        ]

tombstone'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Tombstone -> kv
tombstone'AesonFields (Tombstone did seq time) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "seq" Data.Aeson..= seq
    , Data.Aeson.Key.fromString "time" Data.Aeson..= time
    ]
