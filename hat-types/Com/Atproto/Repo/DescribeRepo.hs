{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Repo.DescribeRepo where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data DescribeRepoResult = DescribeRepoResult
  { collections :: [Data.Text.Text]
  , did :: Data.Text.Text
  , didDoc :: Data.Aeson.Value
  , handle :: Data.Text.Text
  , handleIsCorrect :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON DescribeRepoResult where
  parseJSON = Data.Aeson.withObject "DescribeRepoResult" $ \v -> do
    collections <- v Data.Aeson..: Data.Aeson.Key.fromString "collections"
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    didDoc <- v Data.Aeson..: Data.Aeson.Key.fromString "didDoc"
    handle <- v Data.Aeson..: Data.Aeson.Key.fromString "handle"
    handleIsCorrect <- v Data.Aeson..: Data.Aeson.Key.fromString "handleIsCorrect"
    pure $ DescribeRepoResult collections did didDoc handle handleIsCorrect

instance Data.Aeson.ToJSON DescribeRepoResult where
  toJSON (DescribeRepoResult collections did didDoc handle handleIsCorrect) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "collections" Data.Aeson..= collections
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "didDoc" Data.Aeson..= didDoc
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "handleIsCorrect" Data.Aeson..= handleIsCorrect
        ]
  toEncoding (DescribeRepoResult collections did didDoc handle handleIsCorrect) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "collections" Data.Aeson..= collections
        , Data.Aeson.Key.fromString "did" Data.Aeson..= did
        , Data.Aeson.Key.fromString "didDoc" Data.Aeson..= didDoc
        , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
        , Data.Aeson.Key.fromString "handleIsCorrect" Data.Aeson..= handleIsCorrect
        ]

describeRepoResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeRepoResult -> kv
describeRepoResult'AesonFields (DescribeRepoResult collections did didDoc handle handleIsCorrect) =
  mconcat
    [ Data.Aeson.Key.fromString "collections" Data.Aeson..= collections
    , Data.Aeson.Key.fromString "did" Data.Aeson..= did
    , Data.Aeson.Key.fromString "didDoc" Data.Aeson..= didDoc
    , Data.Aeson.Key.fromString "handle" Data.Aeson..= handle
    , Data.Aeson.Key.fromString "handleIsCorrect" Data.Aeson..= handleIsCorrect
    ]

type DescribeRepo = "com.atproto.repo.describeRepo" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "repo" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] DescribeRepoResult
