{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Server.CheckAccountStatus where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data CheckAccountStatusResult = CheckAccountStatusResult
  { activated :: Bool
  , expectedBlobs :: Integer
  , importedBlobs :: Integer
  , indexedRecords :: Integer
  , privateStateValues :: Integer
  , repoBlocks :: Integer
  , repoCommit :: Data.Text.Text
  , repoRev :: Data.Text.Text
  , validDid :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON CheckAccountStatusResult where
  parseJSON = Data.Aeson.withObject "CheckAccountStatusResult" $ \v -> do
    activated <- v Data.Aeson..: Data.Aeson.Key.fromString "activated"
    expectedBlobs <- v Data.Aeson..: Data.Aeson.Key.fromString "expectedBlobs"
    importedBlobs <- v Data.Aeson..: Data.Aeson.Key.fromString "importedBlobs"
    indexedRecords <- v Data.Aeson..: Data.Aeson.Key.fromString "indexedRecords"
    privateStateValues <- v Data.Aeson..: Data.Aeson.Key.fromString "privateStateValues"
    repoBlocks <- v Data.Aeson..: Data.Aeson.Key.fromString "repoBlocks"
    repoCommit <- v Data.Aeson..: Data.Aeson.Key.fromString "repoCommit"
    repoRev <- v Data.Aeson..: Data.Aeson.Key.fromString "repoRev"
    validDid <- v Data.Aeson..: Data.Aeson.Key.fromString "validDid"
    pure $ CheckAccountStatusResult activated expectedBlobs importedBlobs indexedRecords privateStateValues repoBlocks repoCommit repoRev validDid

instance Data.Aeson.ToJSON CheckAccountStatusResult where
  toJSON (CheckAccountStatusResult activated expectedBlobs importedBlobs indexedRecords privateStateValues repoBlocks repoCommit repoRev validDid) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
        , Data.Aeson.Key.fromString "expectedBlobs" Data.Aeson..= expectedBlobs
        , Data.Aeson.Key.fromString "importedBlobs" Data.Aeson..= importedBlobs
        , Data.Aeson.Key.fromString "indexedRecords" Data.Aeson..= indexedRecords
        , Data.Aeson.Key.fromString "privateStateValues" Data.Aeson..= privateStateValues
        , Data.Aeson.Key.fromString "repoBlocks" Data.Aeson..= repoBlocks
        , Data.Aeson.Key.fromString "repoCommit" Data.Aeson..= repoCommit
        , Data.Aeson.Key.fromString "repoRev" Data.Aeson..= repoRev
        , Data.Aeson.Key.fromString "validDid" Data.Aeson..= validDid
        ]
  toEncoding (CheckAccountStatusResult activated expectedBlobs importedBlobs indexedRecords privateStateValues repoBlocks repoCommit repoRev validDid) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
        , Data.Aeson.Key.fromString "expectedBlobs" Data.Aeson..= expectedBlobs
        , Data.Aeson.Key.fromString "importedBlobs" Data.Aeson..= importedBlobs
        , Data.Aeson.Key.fromString "indexedRecords" Data.Aeson..= indexedRecords
        , Data.Aeson.Key.fromString "privateStateValues" Data.Aeson..= privateStateValues
        , Data.Aeson.Key.fromString "repoBlocks" Data.Aeson..= repoBlocks
        , Data.Aeson.Key.fromString "repoCommit" Data.Aeson..= repoCommit
        , Data.Aeson.Key.fromString "repoRev" Data.Aeson..= repoRev
        , Data.Aeson.Key.fromString "validDid" Data.Aeson..= validDid
        ]

checkAccountStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CheckAccountStatusResult -> kv
checkAccountStatusResult'AesonFields (CheckAccountStatusResult activated expectedBlobs importedBlobs indexedRecords privateStateValues repoBlocks repoCommit repoRev validDid) =
  mconcat
    [ Data.Aeson.Key.fromString "activated" Data.Aeson..= activated
    , Data.Aeson.Key.fromString "expectedBlobs" Data.Aeson..= expectedBlobs
    , Data.Aeson.Key.fromString "importedBlobs" Data.Aeson..= importedBlobs
    , Data.Aeson.Key.fromString "indexedRecords" Data.Aeson..= indexedRecords
    , Data.Aeson.Key.fromString "privateStateValues" Data.Aeson..= privateStateValues
    , Data.Aeson.Key.fromString "repoBlocks" Data.Aeson..= repoBlocks
    , Data.Aeson.Key.fromString "repoCommit" Data.Aeson..= repoCommit
    , Data.Aeson.Key.fromString "repoRev" Data.Aeson..= repoRev
    , Data.Aeson.Key.fromString "validDid" Data.Aeson..= validDid
    ]

type CheckAccountStatus = "com.atproto.server.checkAccountStatus" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] CheckAccountStatusResult
