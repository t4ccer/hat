{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Admin.GetAccountInfo where

import {-# SOURCE #-} qualified Com.Atproto.Admin.Defs
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

newtype GetAccountInfoResult = GetAccountInfoResult
  { getGetAccountInfoResult :: Com.Atproto.Admin.Defs.AccountView
  }

type GetAccountInfo = "com.atproto.admin.getAccountInfo" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetAccountInfoResult
