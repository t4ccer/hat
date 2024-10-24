{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Sync.GetHead where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetHeadResult = GetHeadResult
  { root :: Data.Text.Text
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetHeadResult where
  parseJSON = Data.Aeson.withObject "GetHeadResult" $ \v -> do
    root <- v Data.Aeson..: Data.Aeson.Key.fromString "root"
    pure $ GetHeadResult root

instance Data.Aeson.ToJSON GetHeadResult where
  toJSON (GetHeadResult root) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]
  toEncoding (GetHeadResult root) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "root" Data.Aeson..= root
        ]

getHeadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetHeadResult -> kv
getHeadResult'AesonFields (GetHeadResult root) =
  mconcat
    [ Data.Aeson.Key.fromString "root" Data.Aeson..= root
    ]

type GetHead = "com.atproto.sync.getHead" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetHeadResult
