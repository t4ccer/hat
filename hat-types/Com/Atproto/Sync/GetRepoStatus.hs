{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.GetRepoStatus where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetRepoStatusResult = GetRepoStatusResult
  { active :: Bool
  , did :: Data.Text.Text
  , rev :: Maybe Data.Text.Text
  , status :: Maybe Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRepoStatusResult where
  parseJSON = Data.Aeson.withObject "GetRepoStatusResult" $ \v -> do
    active <- v Data.Aeson..: Data.Aeson.Key.fromString "active"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    rev <- v Data.Aeson..:? Data.Aeson.Key.fromString "rev"
    status <- v Data.Aeson..:? Data.Aeson.Key.fromString "status"
    pure $ GetRepoStatusResult active did rev status

instance Data.Aeson.ToJSON GetRepoStatusResult where
  toJSON (GetRepoStatusResult active did rev status) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "rev" Data.Aeson..?= rev
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]
  toEncoding (GetRepoStatusResult active did rev status) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "rev" Data.Aeson..?= rev
        , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
        ]

getRepoStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRepoStatusResult -> kv
getRepoStatusResult'AesonFields (GetRepoStatusResult active did rev status) =
  mconcat
    [ Data.Aeson.Key.fromString "active" Data.Aeson..= active
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "rev" Data.Aeson..?= rev
    , Data.Aeson.Key.fromString "status" Data.Aeson..?= status
    ]

type GetRepoStatus = "com.atproto.sync.getRepoStatus" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRepoStatusResult
