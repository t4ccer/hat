{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module App.Bsky.Unspecced.Defs where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data SkeletonSearchActor = SkeletonSearchActor
  { did :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonSearchActor where
  parseJSON = Data.Aeson.withObject "SkeletonSearchActor" $ \v -> do
    did <- v Data.Aeson..: Data.Aeson.Key.fromString "did"
    pure $ SkeletonSearchActor did

instance Data.Aeson.ToJSON SkeletonSearchActor where
  toJSON (SkeletonSearchActor did) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]
  toEncoding (SkeletonSearchActor did) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
        ]

skeletonSearchActor'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonSearchActor -> kv
skeletonSearchActor'AesonFields (SkeletonSearchActor did) =
  mconcat
    [ Data.Aeson.Key.fromString "did" Data.Aeson..= did
    ]

data SkeletonSearchPost = SkeletonSearchPost
  { uri :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON SkeletonSearchPost where
  parseJSON = Data.Aeson.withObject "SkeletonSearchPost" $ \v -> do
    uri <- v Data.Aeson..: Data.Aeson.Key.fromString "uri"
    pure $ SkeletonSearchPost uri

instance Data.Aeson.ToJSON SkeletonSearchPost where
  toJSON (SkeletonSearchPost uri) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]
  toEncoding (SkeletonSearchPost uri) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
        ]

skeletonSearchPost'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => SkeletonSearchPost -> kv
skeletonSearchPost'AesonFields (SkeletonSearchPost uri) =
  mconcat
    [ Data.Aeson.Key.fromString "uri" Data.Aeson..= uri
    ]
