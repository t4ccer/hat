{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.GetRepos where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

data GetReposResult = GetReposResult
  { repos :: [GetReposResultReposKind]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetReposResult where
  parseJSON = Data.Aeson.withObject "GetReposResult" $ \v -> do
    repos <- v Data.Aeson..: Data.Aeson.Key.fromString "repos"
    pure $ GetReposResult repos

instance Data.Aeson.ToJSON GetReposResult where
  toJSON (GetReposResult repos) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]
  toEncoding (GetReposResult repos) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
        ]

getReposResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetReposResult -> kv
getReposResult'AesonFields (GetReposResult repos) =
  mconcat
    [ Data.Aeson.Key.fromString "repos" Data.Aeson..= repos
    ]

type GetRepos = "tools.ozone.moderation.getRepos" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "dids" [Data.Text.Text] Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetReposResult
data GetReposResultReposKind
  = GetReposResultReposKindRepoViewDetail Tools.Ozone.Moderation.Defs.RepoViewDetail
  | GetReposResultReposKindRepoViewNotFound Tools.Ozone.Moderation.Defs.RepoViewNotFound
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetReposResultReposKind where
  parseJSON = Data.Aeson.withObject "GetReposResultReposKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "tools.ozone.moderation.defs#repoViewDetail" -> GetReposResultReposKindRepoViewDetail <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "tools.ozone.moderation.defs#repoViewNotFound" -> GetReposResultReposKindRepoViewNotFound <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetReposResultReposKind where
  toJSON = \case
    GetReposResultReposKindRepoViewDetail v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewDetail") <> (Tools.Ozone.Moderation.Defs.repoViewDetail'AesonFields v))
    GetReposResultReposKindRepoViewNotFound v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewNotFound") <> (Tools.Ozone.Moderation.Defs.repoViewNotFound'AesonFields v))
  toEncoding = \case
    GetReposResultReposKindRepoViewDetail v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewDetail") <> (Tools.Ozone.Moderation.Defs.repoViewDetail'AesonFields v))
    GetReposResultReposKindRepoViewNotFound v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "tools.ozone.moderation.defs#repoViewNotFound") <> (Tools.Ozone.Moderation.Defs.repoViewNotFound'AesonFields v))
