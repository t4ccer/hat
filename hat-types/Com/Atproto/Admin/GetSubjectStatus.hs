{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Admin.GetSubjectStatus where

import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import {-# SOURCE #-} qualified Com.Atproto.Repo.StrongRef
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetSubjectStatusResult = GetSubjectStatusResult
  { deactivated :: Maybe Com.Atproto.Admin.Defs.StatusAttr
  , subject :: GetSubjectStatusResultSubjectKind
  , takedown :: Maybe Com.Atproto.Admin.Defs.StatusAttr
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSubjectStatusResult where
  parseJSON = Data.Aeson.withObject "GetSubjectStatusResult" $ \v -> do
    deactivated <- v Data.Aeson..:? Data.Aeson.Key.fromString "deactivated"
    subject <- v Data.Aeson..: Data.Aeson.Key.fromString "subject"
    takedown <- v Data.Aeson..:? Data.Aeson.Key.fromString "takedown"
    pure $ GetSubjectStatusResult deactivated subject takedown

instance Data.Aeson.ToJSON GetSubjectStatusResult where
  toJSON (GetSubjectStatusResult deactivated subject takedown) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "deactivated" Data.Aeson..?= deactivated
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "takedown" Data.Aeson..?= takedown
        ]
  toEncoding (GetSubjectStatusResult deactivated subject takedown) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "deactivated" Data.Aeson..?= deactivated
        , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
        , Data.Aeson.Key.fromString "takedown" Data.Aeson..?= takedown
        ]

getSubjectStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSubjectStatusResult -> kv
getSubjectStatusResult'AesonFields (GetSubjectStatusResult deactivated subject takedown) =
  mconcat
    [ Data.Aeson.Key.fromString "deactivated" Data.Aeson..?= deactivated
    , Data.Aeson.Key.fromString "subject" Data.Aeson..= subject
    , Data.Aeson.Key.fromString "takedown" Data.Aeson..?= takedown
    ]

type GetSubjectStatus = "com.atproto.admin.getSubjectStatus" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "blob" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "did" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "uri" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetSubjectStatusResult
data GetSubjectStatusResultSubjectKind
  = GetSubjectStatusResultSubjectKindRepoRef Com.Atproto.Admin.Defs.RepoRef
  | GetSubjectStatusResultSubjectKindStrongRef Com.Atproto.Repo.StrongRef.StrongRef
  | GetSubjectStatusResultSubjectKindRepoBlobRef Com.Atproto.Admin.Defs.RepoBlobRef
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetSubjectStatusResultSubjectKind where
  parseJSON = Data.Aeson.withObject "GetSubjectStatusResultSubjectKind" $ \v -> do
    v Data.Aeson..: Data.Aeson.Key.fromString "$type" >>= \case
      "com.atproto.admin.defs#repoRef" -> GetSubjectStatusResultSubjectKindRepoRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "com.atproto.repo.strongRef#strongRef" -> GetSubjectStatusResultSubjectKindStrongRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      "com.atproto.admin.defs#repoBlobRef" -> GetSubjectStatusResultSubjectKindRepoBlobRef <$> Data.Aeson.parseJSON (Data.Aeson.Object v)
      _ -> fail "Invalid type"

instance Data.Aeson.ToJSON GetSubjectStatusResultSubjectKind where
  toJSON = \case
    GetSubjectStatusResultSubjectKindRepoRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    GetSubjectStatusResultSubjectKindStrongRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
    GetSubjectStatusResultSubjectKindRepoBlobRef v ->
      Data.Aeson.Object
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoBlobRef") <> (Com.Atproto.Admin.Defs.repoBlobRef'AesonFields v))
  toEncoding = \case
    GetSubjectStatusResultSubjectKindRepoRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoRef") <> (Com.Atproto.Admin.Defs.repoRef'AesonFields v))
    GetSubjectStatusResultSubjectKindStrongRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.repo.strongRef#strongRef") <> (Com.Atproto.Repo.StrongRef.strongRef'AesonFields v))
    GetSubjectStatusResultSubjectKindRepoBlobRef v ->
      Data.Aeson.pairs
        ((Data.Aeson.Key.fromString "$type" Data.Aeson..= "com.atproto.admin.defs#repoBlobRef") <> (Com.Atproto.Admin.Defs.repoBlobRef'AesonFields v))
