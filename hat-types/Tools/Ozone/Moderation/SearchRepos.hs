{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.SearchRepos where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

data SearchReposResult = SearchReposResult
  { cursor :: Maybe Data.Text.Text
  , repos :: [Tools.Ozone.Moderation.Defs.RepoView]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SearchReposResult where
  parseJSON = Data.Aeson.withObject "SearchReposResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    repos <- v Data.Aeson..: Data.Aeson.Key.fromString "repos"
    pure $ SearchReposResult cursor repos

instance Data.Aeson.ToJSON SearchReposResult where
  toJSON (SearchReposResult cursor repos) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]
  toEncoding (SearchReposResult cursor repos) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]

searchReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SearchReposResult -> kv
searchReposResult'AesonFields (SearchReposResult cursor repos) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
    ]

type SearchRepos = "tools.ozone.moderation.searchRepos" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "q" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "term" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] SearchReposResult
