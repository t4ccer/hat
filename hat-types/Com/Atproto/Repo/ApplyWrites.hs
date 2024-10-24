{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.ApplyWrites where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data Create = Create
  { collection :: Data.Text.Text
  , rkey :: Maybe Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Create where
  parseJSON = Data.Aeson.withObject "Create" $ \v -> do
    collection <- v Data.Aeson..: Data.Aeson.Key.fromString "collection"
    rkey <- v Data.Aeson..:? Data.Aeson.Key.fromString "rkey"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ Create collection rkey value

instance Data.Aeson.ToJSON Create where
  toJSON (Create collection rkey value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..?= rkey
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (Create collection rkey value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..?= rkey
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

create'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Create -> kv
create'AesonFields (Create collection rkey value) =
  mconcat
    [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
    , Data.Aeson.Key.fromString "rkey" Data.Aeson..?= rkey
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data CreateResult = CreateResult
  { cid :: Data.Text.Text
  , uri :: Data.Text.Text
  , validationStatus :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON CreateResult where
  parseJSON = Data.Aeson.withObject "CreateResult" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    validationStatus <- v Data.Aeson..:? Data.Aeson.Key.fromString "validationStatus"
    pure $ CreateResult cid uri validationStatus

instance Data.Aeson.ToJSON CreateResult where
  toJSON (CreateResult cid uri validationStatus) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
        ]
  toEncoding (CreateResult cid uri validationStatus) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
        ]

createResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CreateResult -> kv
createResult'AesonFields (CreateResult cid uri validationStatus) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
    ]

data Delete = Delete
  { collection :: Data.Text.Text
  , rkey :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Delete where
  parseJSON = Data.Aeson.withObject "Delete" $ \v -> do
    collection <- v Data.Aeson..: Data.Aeson.Key.fromString "collection"
    rkey <- v Data.Aeson..: Data.Aeson.Key.fromString "rkey"
    pure $ Delete collection rkey

instance Data.Aeson.ToJSON Delete where
  toJSON (Delete collection rkey) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
        ]
  toEncoding (Delete collection rkey) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
        ]

delete'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Delete -> kv
delete'AesonFields (Delete collection rkey) =
  mconcat
    [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
    , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
    ]

data DeleteResult = DeleteResult
  {}
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DeleteResult where
  parseJSON = Data.Aeson.withObject "DeleteResult" $ \v -> do
    pure $ DeleteResult

instance Data.Aeson.ToJSON DeleteResult where
  toJSON (DeleteResult) =
    Data.Aeson.Object $
      mconcat
        []
  toEncoding (DeleteResult) =
    Data.Aeson.pairs $
      mconcat
        []

deleteResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DeleteResult -> kv
deleteResult'AesonFields (DeleteResult) =
  mconcat
    []

data Update = Update
  { collection :: Data.Text.Text
  , rkey :: Data.Text.Text
  , value :: Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Update where
  parseJSON = Data.Aeson.withObject "Update" $ \v -> do
    collection <- v Data.Aeson..: Data.Aeson.Key.fromString "collection"
    rkey <- v Data.Aeson..: Data.Aeson.Key.fromString "rkey"
    value <- v Data.Aeson..: Data.Aeson.Key.fromString "value"
    pure $ Update collection rkey value

instance Data.Aeson.ToJSON Update where
  toJSON (Update collection rkey value) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]
  toEncoding (Update collection rkey value) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
        , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
        , Data.Aeson.Key.fromString "value" Data.Aeson..= value
        ]

update'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Update -> kv
update'AesonFields (Update collection rkey value) =
  mconcat
    [ Data.Aeson.Key.fromString "collection" Data.Aeson..= collection
    , Data.Aeson.Key.fromString "rkey" Data.Aeson..= rkey
    , Data.Aeson.Key.fromString "value" Data.Aeson..= value
    ]

data UpdateResult = UpdateResult
  { cid :: Data.Text.Text
  , uri :: Data.Text.Text
  , validationStatus :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON UpdateResult where
  parseJSON = Data.Aeson.withObject "UpdateResult" $ \v -> do
    cid <- v Data.Aeson..: Data.Aeson.Key.fromString "cid"
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    validationStatus <- v Data.Aeson..:? Data.Aeson.Key.fromString "validationStatus"
    pure $ UpdateResult cid uri validationStatus

instance Data.Aeson.ToJSON UpdateResult where
  toJSON (UpdateResult cid uri validationStatus) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
        ]
  toEncoding (UpdateResult cid uri validationStatus) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
        , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
        ]

updateResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => UpdateResult -> kv
updateResult'AesonFields (UpdateResult cid uri validationStatus) =
  mconcat
    [ Data.Aeson.Key.fromString "cid" Data.Aeson..= cid
    , Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    , Data.Aeson.Key.fromString "validationStatus" Data.Aeson..?= validationStatus
    ]
