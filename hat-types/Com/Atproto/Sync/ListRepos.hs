{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.ListRepos where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data ListReposResult = ListReposResult
  { cursor :: Maybe Data.Text.Text
  , repos :: [Repo]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListReposResult where
  parseJSON = Data.Aeson.withObject "ListReposResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    repos <- v Data.Aeson..: Data.Aeson.Key.fromString "repos"
    pure $ ListReposResult cursor repos

instance Data.Aeson.ToJSON ListReposResult where
  toJSON (ListReposResult cursor repos) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]
  toEncoding (ListReposResult cursor repos) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]

listReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListReposResult -> kv
listReposResult'AesonFields (ListReposResult cursor repos) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
    ]

type ListRepos = "com.atproto.sync.listRepos" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListReposResult
data Repo = Repo
  { active :: Maybe Bool
  , did :: Data.Text.Text
  , head :: Data.Text.Text
  , rev :: Data.Text.Text
  , status :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON Repo where
  parseJSON = Data.Aeson.withObject "Repo" $ \v -> do
    active <- v Data.Aeson..:? Data.Aeson.Key.fromString "active"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    head <- v Data.Aeson..: Data.Aeson.Key.fromString "head"
    rev <- v Data.Aeson..: Data.Aeson.Key.fromString "rev"
    status <- v Data.Aeson..:? Data.Aeson.Key.fromString "status"
    pure $ Repo active did head rev status

instance Data.Aeson.ToJSON Repo where
  toJSON (Repo active did head rev status) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "head" Data.Aeson..= head
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]
  toEncoding (Repo active did head rev status) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "head" Data.Aeson..= head
        , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]

repo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Repo -> kv
repo'AesonFields (Repo active did head rev status) =
  mconcat
    [ Data.Aeson.Key.fromString "active" Data.Aeson..?= active
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "head" Data.Aeson..= head
    , Data.Aeson.Key.fromString "rev" Data.Aeson..= rev
    , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
    ]
